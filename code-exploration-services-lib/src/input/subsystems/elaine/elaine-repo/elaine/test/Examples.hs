module Examples (testAllExamples) where

import qualified Data.Map (fromList)
import Elaine.AST (Value (..))
import Elaine.Exec (Result (..), execRun, pack')
import System.Directory (listDirectory)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    expectationFailure,
    hspec,
    it,
    runIO,
    shouldBe,
    shouldSatisfy,
  )

expected :: String -> Value
expected "state.elaine" = Int 6
expected "id.elaine" = Int 5
expected "abort.elaine" = Unit
expected "safe_division.elaine" = Int 5
expected "implicit.elaine" = String "The answer is: 23"
expected "basics.elaine" = Int 8
expected "structured_logging.elaine" = String "main: msg1\nmain:foo: msg2\nmain:bar: msg3\n"
expected "square_is_even.elaine" = String "The square of 4 is even"
expected "local_reader.elaine" = Int 4
expected "logic.elaine" = String "False, True, True\nFalse, True, False\n"
expected "val.elaine" = Int 32
expected "logic_once.elaine" = String "False, True, True\n"
expected _ = error "Example does not have an expected value"

testAllExamples :: SpecWith ()
testAllExamples = describe "Text Examples" $ do
  examples <- runIO $ listDirectory "examples"

  -- We ignore examples starting with an underscore
  let examples' = filter (\s -> head s /= '_') examples

  mapM_ testExample examples'

run :: String -> String -> Result Value
run path = execRun . pack' . (,) path

testExample :: FilePath -> SpecWith ()
testExample p = do
  contents <- runIO $ readFile $ "examples/" ++ p
  it p $ do
    run p contents `shouldBe` Right (expected p)
