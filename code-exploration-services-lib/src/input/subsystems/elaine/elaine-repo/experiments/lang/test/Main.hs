import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Effen

import qualified Data.Map as Map

example1 :: Comp
example1 = app' (Fun $ \[x] -> If x (return' False) (return' True)) [BoolV True]

example2 :: Comp
example2 = Do (app' plus' [IntV 5]) $ \x -> app' x [IntV 6]

example3 :: Comp
example3 =
  handle'
    (\x -> return' (x, ""))
    [
      ("write", \[s] k ->
        Do (app' k [Unit])      $ \pair ->
        Do (app' fst' [pair]) $ \x    ->
        Do (app' snd' [pair]) $ \acc  ->
        Do (app' concat' [s, acc]) $ \s'   ->
        return' $ pair' x s'
      )
    ]
    (
      Do (op' "write" ["hello"]) $ \_ ->
      Do (op' "write" [" "])     $ \_ ->
      Do (op' "write" ["world"]) $ \_ ->
      return' ()
    )

-- Okay so it's annoying that this works, because it's not algebraic, also not higher-order though
localHandler :: Comp -> Comp
localHandler = handle'
    return'
    [
      ("local", \[s] k ->
        handle'
        return'
        [("read", \_ k -> Do (app' k [s]) return')]
        (Do (app' k [Unit]) return')
      )
    ]

local1 :: Comp
local1 = localHandler $
    Do (op' "local" [StringV "scope1"]) $ \_ ->
    Do (op' "read" [Unit]) $ \x ->
    return' x

testEval :: Test.HUnit.Test
testEval = TestList $ map mkEvalTest
  [
    ("Not", BoolV False, example1),
    ("BuiltInPlus", IntV 11, example2),
    ("Write", Pair Unit (StringV "hello world"), example3),
    ("Local1", StringV "scope1", local1)
  ]
  where
    mkEvalTest (name, val, comp) = 
      TestLabel name $ TestCase $ assertEqual
        (name ++ " failed")
        val (eval comp)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList
  [
    testEval
  ]
