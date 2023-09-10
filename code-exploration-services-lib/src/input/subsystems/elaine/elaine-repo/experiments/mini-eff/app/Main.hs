{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind (Type, Constraint)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.ST.Lazy (strictToLazyST)

data Queue m a b where
  End :: (a -> m b) -> Queue m a b
  Cons :: (a -> m x) -> Queue m x b -> Queue m a b

tsingleton :: (a -> m b) -> Queue m a b
tsingleton = End

(|>) :: Queue m a x -> (x -> m b) -> Queue m a b
(Cons t r) |> r' = Cons t (r |> r')
(End t)    |> r' = Cons t (End r')

(><) :: Queue m a x -> Queue m x b -> Queue m a b
(Cons t r) >< t2 = Cons t (r >< t2)
(End a)    >< t2 = Cons a t2

data Union (r :: [Type -> Type]) a where
  Union :: Word -> t a -> Union r a

decomp :: Union (t ': r) a -> Either (Union r a) (t a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a

newtype P t r = P { unP :: Word }

class FindElem (t :: Type -> Type) (r :: [Type -> Type]) where
  elemNo :: P t r

instance FindElem t (t : r) where
  elemNo = P 0

instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' : r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)

class FindElem eff effs => Member (eff :: Type -> Type) effs where
  inj :: eff a -> Union effs a
  prj :: Union effs a -> Maybe (eff a)

unsafeInj :: Word -> t a -> Union r a
unsafeInj = Union

unsafePrj :: Word -> Union r a -> Maybe (t a)
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing

instance FindElem t r => Member t r where
  inj = unsafeInj $ unP (elemNo :: P t r)
  prj = unsafePrj $ unP (elemNo :: P t r)

class Member t r => t <: r
instance Member t r => t <: r

-- Cool syntax sugar for multiple effects
type family Members effs effs' :: Constraint where
    Members (eff ': effs) effs' = (Member eff effs', Members effs effs')
    Members '[] effs' = ()

type family effs <:: effs' :: Constraint where
    (eff : effs) <:: effs' = (Member eff effs', effs <:: effs')
    '[] <:: effs' = ()

-- Effectful arrow type
type Arr effs a b = a -> Eff effs b

-- Composition of effectful arrows
type Arrs effs a b = Queue (Eff effs) a b

-- The effect monad
data Eff effs a
  = Val a
  | forall b. E (Union effs b) (Arrs effs b a)

instance Functor (Eff effs) where
  fmap f (Val x) = Val (f x)
  fmap f (E u q) = E u (q |> (Val . f))

instance Applicative (Eff effs) where
  pure = Val
  Val f <*> Val x = Val $ f x
  Val f <*> E u q = E u (q |> (Val . f))
  E u q <*> m     = E u (q |> (`fmap` m))

instance Monad (Eff effs) where
  Val x >>= k = k x
  E u q >>= k = E u (q |> k)

send :: eff <: effs => eff a -> Eff effs a
send t = E (inj t) (tsingleton Val)

qApp :: Arrs effs b w -> b -> Eff effs w
qApp q' x = case q' of
  End k  -> k x
  Cons k t -> case k x of
    Val y -> qApp t y
    E u q -> E u (q >< t)

qComp :: Arrs effs a b -> (Eff effs b -> Eff effs' c) -> Arr effs' a c
qComp g h a = h $ qApp g a

run :: Eff '[] a -> a
run (Val x) = x
run _       = error "Internal:run - This (E) should never happen"

-- State
data State s r where
  Get :: State s s
  Put :: s -> State s ()

get :: State s <: effs => Eff effs s
get = send Get

put :: State s <: effs => s -> Eff effs ()
put s = send (Put s)

runState :: s -> Eff (State s ': effs) a -> Eff effs (a, s)
runState s0 = handleRelayS s0 (\s x -> pure (x, s)) $ \s x k -> case x of
  Get -> k s s
  Put s' -> k s' ()

-- Fresh
data Fresh r where
  Fresh :: Fresh Int

fresh :: Member Fresh effs => Eff effs Int
fresh = send Fresh

runFresh :: Int -> Eff (Fresh : effs) a -> Eff effs (a, Int)
runFresh s = handleRelayS s (\s' a -> pure (a, s')) (\s' Fresh k -> (k $ s' + 1) s')

-- Console
data Console r where
    Write :: Show a => a -> Console ()

write :: (Console <: effs, Show a) => a -> Eff effs ()
write = send . Write

runConsolePure :: Eff (Console : effs) a -> Eff effs (a, [String])
runConsolePure = handleRelayS [] (\s' a -> pure (a, s')) $ \s x k -> case x of
    Write x -> k (s ++ [show x]) ()

runConsoleIO :: Eff (Console : effs) a -> Eff effs (IO a)
runConsoleIO = handleRelay (pure . pure) $ \x k -> case x of
    Write str -> do
        -- This was the result of a lot of experimentation, why does this work?
        -- And does is need to be this way?
        -- If we map the print to Eff eff (IO a), it's a Val and hence discarded immediately
        -- That is do { pure $ print str ; k () }, will discard the info in the monad and
        -- replace it with the value from k ().
        x <- k ()
        pure $ do
            print str
            x

-- | Parameterized 'handleRelay'. Allows sending along some state of type
-- @s :: *@ to be handled for the target effect, or relayed to a handler that
-- can- handle the target effect.
handleRelayS
  :: s
  -> (s -> a -> Eff effs b)
  -- ^ Handle a pure value.
  -> (forall v. s -> eff v -> (s -> Arr effs v b) -> Eff effs b)
  -- ^ Handle a request for effect of type @eff :: * -> *@.
  -> Eff (eff : effs) a
  -> Eff effs b
  -- ^ Result with effects of type @eff :: * -> *@ handled.
handleRelayS s' ret h = loop s'
  where
    loop s (Val x)  = ret s x
    loop s (E u' q) = case decomp u' of
        Right x -> h s x k
        Left  u -> E u (tsingleton (k s))
      where
        k s'' x = loop s'' $ qApp q x

handleRelay :: (a -> Eff effs b)
            -> (forall v. eff v -> Arr effs v b -> Eff effs b)
            -> Eff (eff : effs) a
            -> Eff effs b
handleRelay ret h = loop
  where
    loop (Val x) = ret x
    loop (E u' q) = case decomp u' of
      Right x -> h x k
      Left u -> E u (tsingleton k)
      where
        k = qComp q loop
        
example1 :: [State Int, Fresh, Console] <:: effs => Eff effs Int
example1 = do
  i <- fresh     -- i=0
  i <- fresh     -- i=1
  i <- fresh     -- i=2
  write i
  put i          -- i=2, state=2
  i <- fresh     -- i=3, state=2
  j <- get       -- i=3, j=2, state=2
  write (i + j)
  Val (i + j) -- expected: 3 + 2 = 5

map' :: (a -> Eff e b) -> [a] -> Eff e [b]
map' f [] = pure []
map' f (a:as) = do
    -- this is annoying, we need to bind everything, cause monads
    -- luckily the type system helps us a bit
    -- I wonder if this holds back optimizations in GHC.
    b <- f a
    bs <- map' f as 
    -- The pure is annoying as well
    -- This has some similarities to Ok wrapping in Rust
    pure (b:bs)

foldr' :: (a -> b -> Eff e b) -> b -> [a] -> Eff e b
foldr' acc init [] = pure init
foldr' acc init (a:as) = do
    b <- foldr' acc init as
    acc a b

foo :: Console <: e => Int -> Eff e Int
foo a = do
    write a
    pure (a + 1)

example2 :: Console <: e => Eff e [Int]
example2 = do
  let i = [1..5]
  map' foo i

bar :: IO Int
bar = do
    print "5"
    pure 5

composition :: (a -> Eff e b) -> (b -> Eff e c) -> (a -> Eff e c)
composition f g x = do
   b <- f x
   g b

setState :: State a <: e => a -> Eff e a
setState i = do
    put i
    pure i

writeLong :: (Console <: e, Show a) => a -> Eff e a
writeLong i = do
    write ("The value is: " ++ show i)
    pure i

example3 :: [State Int, Console] <:: e => Eff e Int
example3 = composition setState writeLong 3

example4 :: [State Int, Console] <:: e => Eff e ()
example4 = do
    let i = [1..3::Int]
    -- We expect: 3 + (3 + 2) + (3 + 2 + 1) = 14
    final <- foldr'
        (\a b -> do
            n <- get
            put (n + a + b)
            pure (a + b))
        0
        i
    x::Int <- get
    write x

handleSecrets :: e <:: [State Int, Fresh] => (String -> Eff e a) -> Eff e a
handleSecrets f = f "SOME SECRET STRING"

example5 :: Eff '[] String
example5 = handleSecrets (pure . reverse)

-- -- Fails to compile by design, because we are not allowed to leak the secret
-- example6 :: Eff '[Console] ()
-- example6 = handleSecrets (write . reverse)

main :: IO ()
main = do
    printDivider "Example 1: IO"
    run $ runConsoleIO $ runState (0::Int) $ runFresh 0 example1
    
    printDivider "Example 1: Pure"
    print $ snd $ run $ runConsolePure $ runState (0::Int) $ runFresh 0 example1

    printDivider "Example 2"
    xs <- run $ runConsoleIO example2
    print xs

    printDivider "Example 3"
    res <- run $ runConsoleIO $ runState (0::Int) example3
    putStr "The state was: "
    print $ snd res

    printDivider "Example 4"
    run $ runConsoleIO $ runState (0::Int) example4
    pure ()

-- main = do
--     print $ run $ runConsolePure $ runFresh 0 $ runState (0::Int) example

printDivider :: String -> IO ()
printDivider name = do
    putStrLn "================================================="
    putStrLn name
    putStrLn "=================================================" 
