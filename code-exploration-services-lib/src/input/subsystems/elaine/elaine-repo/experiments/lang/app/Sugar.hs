{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Sugar where
import AST
import Prelude hiding ((<=), return)
import Data.Set as Set hiding (map)
import Data.Map as Map hiding (map)

(#) :: Ident -> TypeVal -> TypedVar
(#) = TypedVar

(!) :: TypeVal -> Set Op -> TypeComp
(!) = TypeComp

infixr 0 &
(&) :: ToRow a => a -> Comp -> Comp
row & c2 = case toRow row of 
  DoRow x c1 -> Do x c1 c2

data DoRow = DoRow (Maybe Ident) Comp

class ToRow a where
  toRow :: a -> DoRow

instance ToRow Comp where
  toRow = DoRow Nothing

instance ToRow DoRow where
  toRow = id

infixl 1 <=
(<=) :: Ident -> Comp -> DoRow
"_" <= comp = DoRow Nothing comp
x <= comp = DoRow (Just x) comp

class IntoVal a where
  val :: a -> Val

instance IntoVal Val where
  val = id

instance IntoVal String where
  val = ValStr

instance IntoVal Int where 
  val = ValInt . fromIntegral

instance IntoVal Bool where
  val = ValBool

instance IntoVal a => IntoVal [a] where
  val as = ValTuple $ map val as

instance IntoVal () where
  val () = ValUnit

instance (IntoVal a, IntoVal b) => IntoVal (a,b) where
  val (a, b) = ValTuple [val a, val b]

instance (IntoVal a, IntoVal b, IntoVal c) => IntoVal (a,b,c) where
  val (a, b, c) = ValTuple [val a, val b, val c]

-- Not sure how to do just (val 5), which requires a type hint
int :: Int -> Val
int = val

var :: String -> Val
var = ValVar

dFun :: String -> [TypedVar] -> TypeComp -> Comp -> Decl
dFun = DeclFun

dOp :: String -> [TypeVal] -> TypeVal -> Operation
dOp ident params ret = Operation ident (TypeOp params ret)

dEffect :: String -> [Operation] -> Decl
dEffect = DeclEffect

if' :: IntoVal a => a -> Comp -> Comp -> Comp
if' a = If (val a)

handle :: Val -> Comp -> Comp
handle = Handle

elaborate :: Val -> Comp -> Comp
elaborate = Elaborate

return :: IntoVal a => a -> Comp
return = Return . val

resume :: IntoVal a => a -> Comp
resume v = App (var "$k") [val v]

lambda :: [Ident] -> Comp -> Val
lambda = ValFun

app :: IntoVal a => a -> [Val] -> Comp
app a = App (val a)

op :: Ident -> [Val] -> Comp
op x vs = Op x vs (Return (var "$y"))

data HandlerBranch
  = HandlerReturn Ident Comp
  | HandlerOp Ident HandleOp

data PartialHandler = PartialHandler (Maybe (Ident, Comp)) [(Ident, HandleOp)]
on :: Ident -> [Ident] -> Comp -> HandlerBranch
on op args comp = HandlerOp op (HandleOp args comp)

onReturn :: Ident -> Comp -> HandlerBranch 
onReturn = HandlerReturn

handler :: Ident -> [HandlerBranch] -> Val
handler ident branches = case ret of
  Just ret -> ValHan ident ret (Map.fromList ops)
  Nothing -> ValHan ident ("x", Return (var "x")) (Map.fromList ops)
  where
    PartialHandler ret ops = partialHandler branches

partialHandler :: [HandlerBranch] -> PartialHandler
partialHandler [] = PartialHandler Nothing []
partialHandler ((HandlerReturn i c):bs) = PartialHandler (Just (i, c)) ops
  where
    PartialHandler _ ops = partialHandler bs
partialHandler ((HandlerOp i h):bs) = PartialHandler ret ((i, h):ops)
  where
    PartialHandler ret ops = partialHandler bs

elaboration :: Ident -> [HandlerBranch] -> Val
elaboration ident branches = case ret of
  Just ret -> error "cannot specify return branch of an elaboration"
  Nothing -> ValElab ident (Map.fromList ops)
  where
    PartialHandler ret ops = partialHandler branches
