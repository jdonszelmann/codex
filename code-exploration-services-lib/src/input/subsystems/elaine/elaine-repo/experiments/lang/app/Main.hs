module Main where

import Prelude hiding ((<=), return, fst, snd, concat)
import Effen
import AST
import Sugar

fst :: Val
fst = ValBuiltIn $ BuiltIn "fst" (\[ValTuple [a, b]] -> Return a)

snd :: Val
snd = ValBuiltIn $ BuiltIn "snd" (\[ValTuple [a, b]] -> Return b)

concat :: Val
concat = ValBuiltIn $ BuiltIn "concat" (\[ValStr a, ValStr b] -> Return $ ValStr $ a ++ b)

example3 :: Comp
example3 =
  handle (
    handler "write" [
      onReturn "x" $ return (var "x", ""),
      on "write" ["s"] (
        "pair" <= resume () &
        "x"    <= app fst [var "pair"] &
        "acc"  <= app snd [var "pair"] &
        "s'"   <= app concat [var "s", var "acc"] &
        return [var "x", var "s'"]
      )
    ]
  )
  (
    op "write" [val "hello"] &
    op "write" [val " "] &
    op "write" [val "world"] &
    return ()
  )

-- localHandler :: Comp -> Comp
-- localHandler =
--   handle'
--     return'
--     [ ( "local",
--         \[s] k ->
--           handle'
--             return'
--             [("read", \_ k -> Do (app' k [s]) return')]
--             (Do (app' k [Unit]) return')
--       )
--     ]

-- local1 :: Comp
-- local1 = localHandler $
--   Do (op' "local" ["scope1"]) $ \_ ->
--     Do (op' "local" ["scope2"]) $ \_ ->
--       Do (op' "read" [Unit]) $ \x ->
--         return' x

-- localElab :: Comp -> Comp
-- localElab =
--   elaborate'
--     [ ( "local",
--         \[c] k ->
--           Do c $ \s ->
--             handle'
--               return'
--               [("read", \_ k -> Do (app' k [s]) return')]
--               (Do (app' k [Unit]) return')
--       )
--     ]

-- local2 :: Comp
-- local2 = localElab $
--   Do (oph' "local" [return' "scope1"]) $ \_ ->
--     Do (oph' "local" [return' "scope2"]) $ \_ ->
--       Do (op' "read" [Unit]) return'

main :: IO ()
main = do
  _ <- eval' example3
  pure ()
