{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (pack)
import Elaine.AST
import Elaine.Eval (evalExpr, newEnv)
import Elaine.Exec (Command (..), Result (..), exec, execParse, execRun, pack')
import Elaine.Ident (Ident (..), Location (..))
import Elaine.Parse (parseExpr)
import Elaine.Transform (elabToHandle)
import Examples (testAllExamples)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Text.RawString.QQ
import TypeCheck (testTypeCheck)

tt :: Expr
tt = Val $ Bool True

ff :: Expr
ff = Val $ Bool False

iv :: Int -> Expr
iv = Val . Int

-- Check transformed
-- shouldEvalTo :: String -> Value -> Expectation
-- x `shouldEvalTo` e = case prettyError (parseProgram x) of
--   Left a -> expectationFailure a
--   Right parsed -> do
--     putStrLn "======= Original ======="
--     putStrLn (pretty parsed)
--     exec parsed `shouldBe` e
--     let transformed = elabTrans parsed
--     putStrLn "====== Transformed ======"
--     putStrLn (pretty transformed)
--     exec transformed `shouldBe` e

parse :: String -> Result Program
parse = execParse . pack' . (,) "test"

run :: String -> Result Value
run = execRun . pack' . (,) "test"

main :: IO ()
main = hspec $ do
  testParseExpr
  testTypeCheck
  testEval
  testAllExamples

-- testExec

testParseExpr :: SpecWith ()
testParseExpr = describe "parseExpr" $ do
  let f = parseExpr
  it "parses values" $ do
    f "false" `shouldBe` Right (Val $ Bool False)
    f "true" `shouldBe` Right (Val $ Bool True)
    f "10" `shouldBe` Right (Val $ Int 10)
    f "\"hello\"" `shouldBe` Right (Val $ String "hello")
    f "()" `shouldBe` Right (Val Unit)

  it "parses applications" $ do
    f "hello()(5)" `shouldBe` Right (App (App (Var (Ident "hello" (LocOffset 0))) []) [Val $ Int 5])

  it "parses if expressions" $ do
    f "if true { false } else { true }" `shouldBe` Right (If tt ff tt)
    f "if true { 5 } else { if false { 6 } else { 7 } }" `shouldBe` Right (If tt (iv 5) (If ff (iv 6) (iv 7)))

  it "parses handle expressions" $ do
    f "handle[h] { if true { true } else { true } }" `shouldBe` Right (Handle (Var (Ident "h" (LocOffset 7))) (If tt tt tt))

  it "parses let expressions" $ do
    f "{ let x = if true { true } else { true }; x }" `shouldBe` Right (Let (Just (Ident "x" (LocOffset 6))) Nothing (If tt tt tt) (Var (Ident "x" (LocOffset 40))))

  it "treats braces transparently" $ do
    f "{{{{ if {{ true }} {{ true }} else {{ true }} }}}}" `shouldBe` Right (If tt tt tt)

testEval :: SpecWith ()
testEval = describe "eval0" $ do
  let f = evalExpr newEnv
  let x = Ident "x" LocNone
  let y = Ident "y" LocNone
  let z = Ident "z" LocNone
  it "gives reduces if expressions" $ do
    f (If tt tt ff) `shouldBe` Bool True
    f (If ff tt ff) `shouldBe` Bool False
    f (If tt ff tt) `shouldBe` Bool False
    f (If ff ff tt) `shouldBe` Bool True
  it "binds simple lets" $ do
    f (Let (Just x) Nothing (iv 5) $ Var x) `shouldBe` Int 5
    f (Let (Just x) Nothing (iv 5) $ Var x) `shouldBe` Int 5

  it "keeps shadowing bindings intact" $ do
    f (Let (Just x) Nothing (iv 5) $ Let (Just x) Nothing (iv 6) (Var x)) `shouldBe` Int 6

  it "applies function arguments" $ do
    f (App (Val $ lam [x] $ Var x) [iv 5]) `shouldBe` Int 5
    f (App (Val $ lam [y] $ Var y) [If tt (iv 5) (iv 6)]) `shouldBe` Int 5
    f (App (Val $ lam [x, y] (Var y)) [iv 5, iv 10]) `shouldBe` Int 10

  it "applies multiple function arguments" $ do
    let g = Val $ lam [x, y, z] (If (Var x) (Var y) (Var z))
    f (App g [tt, iv 100, iv 200]) `shouldBe` Int 100
    f (App g [ff, iv 100, iv 200]) `shouldBe` Int 200
    f (App g [App g [tt, tt, ff], iv (-10), iv 10]) `shouldBe` Int (-10)

  it "handles an abort-like effect" $ do
    let abort = Ident "abort" LocNone
    -- Returns a boolean to signify whether it ran to completion or aborted
    let h = Val $ Hdl $ Handler (Function [(x, Nothing)] Nothing tt) [OperationClause abort [] ff]
    f (Handle h (Let (Just x) Nothing (iv 5) (Var x))) `shouldBe` Bool True
    f (Handle h (Let (Just x) Nothing (App (Var abort) []) (Var x))) `shouldBe` Bool False

testRun :: SpecWith ()
testRun = describe "run" $ do
  it "interprets a simple main module" $ do
    run "let main = 5;"
      `shouldBe` Right (Int 5)

    run
      [r|
      let main = {
        let x = 5;
        let y = 6;
        if true { x } else { y }
      };
    |]
      `shouldBe` Right (Int 5)

  it "elaborates higher order effects into values" $ do
    run
      [r|
      effect A! {
        a!() Int
      }

      let elabA = elaboration A! -> <> {
        a!() {
          10
        }
      };

      let main = {
        elab[elabA] { a!() }
      };
    |]
      `shouldBe` Right (Int 10)

  it "elaborates higher order effects into algebraic effects" $ do
    run
      [r|
      effect A {
        a() Int
      }

      effect B! {
        b!() Int
      }

      let eB = elaboration B! -> <A> {
        b!() {
          a()
        }
      };

      let main = {
        let h = handler {
          return(x) {
            x
          }
          a() {
            101
          }
        };
        handle[h] {
          elab[eB] {
            b!()
          }
        }
      };
    |]
      `shouldBe` Right (Int 101)

  it "can access global values" $ do
    run
      [r|
      let x = 5;
      let y = 6;
      let main = if true { x } else { y };
    |]
      `shouldBe` Right (Int 5)

  it "can use functions from the standard library" $ do
    run
      [r|
      use std;
      let main = add(6, 5);
    |]
      `shouldBe` Right (Int 11)

    run
      [r|
      use std;
      let main = sub(6, 5);
    |]
      `shouldBe` Right (Int 1)

    run
      [r|
      use std;
      let main = mul(6, 5);
    |]
      `shouldBe` Right (Int 30)

    run
      [r|
      use std;
      let main = concat(concat("hello", " "), "world");
    |]
      `shouldBe` Right (String "hello world")

    run
      [r|
      use std;
      let main = and(true, true);
    |]
      `shouldBe` Right (Bool True)

  it "a" $ do
    run
      [r|
      use std;
      let main = {
        let x = add(5, 10);
        let y = add(x, 2);
        mul(2, y)
      };
    |]
      `shouldBe` Right (Int 34)

  it "handles the Out effect" $ do
    run
      [r|
      use std;

      effect Out {
        write(String) ()
      }

      let h = handler {
        return(x) {
          ""
        }
        write(m) {
          let rest = resume(());
          concat(m, rest)
        }
      };

      let main = handle[h] {
        write("hello");
        write(" ");
        write("world");
        ()
      };
    |]
      `shouldBe` Right (String "hello world")

  it "can turn values into strings" $ do
    run
      [r|
      use std;
      let main = show_int(5);
    |]
      `shouldBe` Right (String "5")

  it "can construct data types" $ do
    run
      [r|
      type Either {
        Left(<> a)
        Right(<> b)
      }

      let main = Left(5);
    |]
      `shouldSatisfy` \case
        Right (Data (Ident "Either" _) (Ident "Left" _) [Val (Int 5)]) -> True
        _ -> False

  it "can represent a list" $ do
    run
      [r|
      type List {
        Nil()
        Cons(a, List)
      }

      let main = Cons(1, Cons(2, Nil()));
    |]
      `shouldSatisfy` \case
        Right (Data (Ident "List" _) (Ident "Cons" _) [Val (Int 1), Val (Data (Ident "List" _) (Ident "Cons" _) [Val (Int 2), Val (Data (Ident "List" _) (Ident "Nil" _) [])])]) -> True
        _ -> False

  it "can match on custom data types" $ do
    run
      [r|
      type Bool {
        False()
        True()
      }

      let main = match True() {
        True() => 5
        False() => 10
      };
    |]
      `shouldBe` Right (Int 5)

  it "can call a defined function" $ do
    run
      [r|
      use std;
      let add3 = fn(a: Int, b: Int, c: Int) <> Int {
        add(a, add(b, c))
      };

      let main = add3(1, 2, 3);
    |]
      `shouldBe` Right (Int 6)

  it "can call a function returning a custom datatype" $ do
    run
      [r|
      use std;

      type Maybe {
        Just(<> a)
        Nothing()
      }

      let safediv = fn(x: Int, y: Int) <> Maybe {
        if eq(y, 0) {
          Nothing()
        } else {
          Just(div(x,y))
        }
      };

      let main = safediv(6, 0);
    |]
      `shouldSatisfy` \case
        Right (Data (Ident "Maybe" _) (Ident "Nothing" _) []) -> True
        _ -> False

  it "can handle the abort effect" $ do
    run
      [r|
      use std;
      
      type Maybe {
        Just(<> a)
        Nothing()
      }

      effect Abort {
        abort() ()
      }

      let hAbort = handler {
        return(x) { Just(x) }
        abort() { Nothing() }
      };

      let safediv = fn(x: <> Int, y: <> Int) <Abort> Int {
        if eq(y, 0) {
          abort()
        } else {
          div(x, y)
        }
      };

      let main = match handle[hAbort] {
        let x = 6;
        let y = 0;
        safediv(x, y)
      } {
        Nothing() => "cannot divide by zero"
      };
    |]
      `shouldBe` Right (String "cannot divide by zero")

  it "can elaborate and handle exceptions" $ do
    run
      [r|
      use std;

      type Maybe {
        Just(<> a)
        Nothing()
      }

      effect Abort {
        abort() ()
      }

      effect Exc! {
        catch!(a) a
        throw!() b
      }

      let h = handler {
        return(x) { Just(x) }
        abort() { Nothing() }
      };

      let e = elaboration Exc! -> <Abort> {
        catch!(e) {
          handle[h] e
        }
        throw!() {
          abort()
        }
      };

      let safediv = fn(x: Int, y: Int) <Exc!> Int {
        if eq(y, 0) {
          throw!()
        } else {
          div(x, y)
        }
      };

      let main = handle[h] {
        elab[e] {
          match catch!(safediv(5, 0)) {
            Nothing() => "cannot divide by 0"
          }
        }
      };
    |]
      `shouldSatisfy` \case
        Right (Data (Ident "Maybe" _) (Ident "Just" _) [Val (String "cannot divide by 0")]) -> True
        _ -> False

  it "can do a state effect" $ do
    run
      [r|
      use std;

      effect State {
        get() Int
        put(Int) ()
      }

      let h = handler {
        return(x) {
          fn(n: Int) <> a { x }
        }
        get() {
          fn(n: Int) <> a {
            resume(n)(n)
          }
        }
        put(x) {
          fn(n: Int) <> a { 
            resume(())(x)
          } 
        }
      };

      let main = {
        let f = handle[h] {
          let x = get();
          put(add(x, 1));
          get()
        };
        f(5)
      };
    |]
      `shouldBe` Right (Int 6)

  it "can do some funky stuff" $ do
    run
      [r|
      use std;
      let hVal = handler {
        return(x) { x }
        val(f) {
          resume(fn() <> Int { f(5) })
        }
      };
      let timesTwo = fn(x: Int) <> Int {
        mul(2, x)
      };
      let main = handle[hVal] {
        val(timesTwo)()
      };
    |]
      `shouldBe` Right (Int 10)

  it "can do the reader effect" $ do
    run
      [r|
      use std;

      effect Reader! {
        ask!() a
        # TODO fix function type
        local!(f, b) b
      }

      effect Reader {
        ask() a
      }

      let h = fn(x: a) <> c {
        handler {
          return(y) { y }
          ask() { resume(x) }
        }
      };

      let eReader = elaboration Reader! -> <Reader> {
        ask!() { ask() }
        local!(f, c) {
          handle[h(f(ask()))] { c }
        }
      };

      type Triple {
        t(Int, Int, Int)
      }

      let double = fn(x: Int) <> Int {
        mul(2, x)
      };

      let main = {
        handle[h(1)] elab[eReader] {
          let x = ask!();
          local!(double, {
            let y = ask!();
            local!(double, {
              let z = ask!();
              t(x, y, z)
            })
          })
        }
      };
    |]
      `shouldSatisfy` \case
        Right (Data (Ident "Triple" _) (Ident "t" _) [Val (Int 1), Val (Int 2), Val (Int 4)]) -> True
        _ -> False

  it "can do the log effect" $ do
    run
      [r|
      use std;

      effect Log! {
        context!(String, a) a
        log!(String) ()
      }

      effect Writer {
        write(String) ()
      }

      effect Reader {
        ask() String
      }

      let hWrite = handler {
        return(x) { "" }
        log(x) {
          let line = concat(x, "\n");
          concat(line, resume(()))
        }
      };

      # TODO handler type
      let hRead = fn(x: <> String) <> h {
        handler {
          return(y) { y }
          ask() { resume(x) }
        }
      };

      let eLog = elaboration Log! -> <Writer,Reader> {
        context!(s, c) {
          let newCtx = concat(concat(ask(), ":"), s);
          handle[hRead(newCtx)] c
        }
        log!(s) { 
          let msg = concat(concat(ask(), ": "), s);
          log(msg)
        }
      };

      let main = handle[hRead("root")] handle[hWrite] elab[eLog] {
        log!("one");
        context!("foo", {
          log!("two");
          context!("bar", {
            log!("three")
          })
        })
      };
    |]
      `shouldBe` Right (String "root: one\nroot:foo: two\nroot:foo:bar: three\n")

  it "does an use" $ do
    run
      [r|
    mod math {
      use std;
      pub let double = fn(x) {
        mul(2, x)
      };
      pub let abs = fn(x) {
        if lt(x, 0) {
          sub(0, x)
        } else {
          x
        }
      };
    }
    
    use math;
    let main = abs(double(-10));
    |]
      `shouldBe` Right (Int 20)

  it "elaborates dynamically based on if" $ do
    run
      [r|
      let x = true;
      # TODO should register the effects for type checking 
      let e1 = elaboration A! -> <> {
        a!() { 5 }
      };
      let e2 = elaboration A! -> <> {
        a!() { 10 }
      };
      let e = if x { e1 } else { e2 };
      let main = elab[e] { a!() };
    |]
      `shouldBe` Right (Int 5)

  it "uses the right elaboration based on runtime values" $ do
    run
      [r|
    use std;

    let f = fn(x) { a!() };
    let e1 = elaboration A! -> <> {
      a!() { 5 }
    };
    let e2 = elaboration A! -> <> {
      a!() { 10 }
    };
    let main = elab[e1] {
      let g = elab[e2] {
        if false {
          f
        } else {
          let y = f();
          fn(x) { add(y, 1) }
        }
      };
      g()
    };
    |]
      `shouldBe` Right (Int 11)

  it "applies the inner handler" $ do
    run
      [r|
      let e1 = elaboration A! -> <> {
        a!() { 5 }
      };
      let e2 = elaboration A! -> <> {
        a!() { 10 }
      };

      let main = elab[e1] {
        let f = fn(x) { elab[e2] x() };
        let g = fn() { a!() };
        f(g)
      };
    |]
      `shouldBe` Right (Int 10)

  it "weird uses" $ do
    run
      [r|
    mod A {
      pub mod B {
        pub mod C {
          pub let x = 5;
        }
      }
    }

    use A;
    use B;
    use C;
    let main = x;
    |]
      `shouldBe` Right (Int 5)

  it "weird use 2" $ do
    run
      [r|
    mod A {
      mod B {
        pub let x = 5;
      }

      pub mod C {
        pub use B;
      }
    }

    use A;
    use C;
    let main = x;
    |]
      `shouldBe` Right (Int 5)

-- TODO expression context for elab
-- it "applies elaboration with parameter" $ do
--   run [r|
--     let e1 = elaboration A! -> <> {
--       a!() { 5 }
--     };
--     let e2 = elaboration A! -> <> {
--       a!() { 10 }
--     };
--     let e3 = elaboration B! -> <> {
--       b!() { () }
--     };

--     let main = elab[e1] elab[e2] { a!() };

--     let main = elab {
--       let f = fn(x) { elab[e2] x() };
--       let g = fn() { a!() };
--       f(g)
--     };
--   |]
--     `shouldBe` Right (Int 10)

--   run [r|
--     let ex = fn(x) {
--       elaboration A! -> <> {
--         a!() { x }
--       }
--     };

--     let f = ex(5);

--     let main = elab[f] {
--       a!()
--     };
--   |]
--     `shouldBe` Right (Int 5)