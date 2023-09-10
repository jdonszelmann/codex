{-# LANGUAGE QuasiQuotes #-}

module TypeCheck (testTypeCheck) where

import Data.Bifunctor (second)
import Data.Either (isLeft)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Text.Internal.Read (IParser (P))
import Elaine.Exec (Result, exec, execCheck, isTypeError, pack')
import Elaine.Ident (Ident (..), Location (LocNone))
import Elaine.Parse (ParseResult, parseExpr, parseProgram)
import Elaine.Pretty (pretty)
import Elaine.Transform (elabToHandle)
import Elaine.TypeCheck (TypeEnv (TypeEnv), getMain, getVar, runInfer, typeCheck, unifyRows)
import Elaine.TypeVar (TypeVar (ExplicitVar))
import Elaine.Types
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
import Test.Hspec.Runner (SpecResult (specResultSuccess))
import Text.RawString.QQ (r)
import Prelude hiding (pure)

check :: String -> Result CompType
check = execCheck . pack' . (,) "test"

pure :: ValType -> CompType
pure = CompType rowEmpty

testTypeCheck :: SpecWith ()
testTypeCheck = describe "typeCheck" $ do
  it "checks an if" $ do
    check
      [r|
      let main = if true { 5 } else { 10 };
    |]
      `shouldBe` Right (pure TypeInt)

  it "checks bindings" $ do
    check
      [r|
      let x = "Hello";
      let main = x;
    |]
      `shouldBe` Right (pure TypeString)

  it "checks function applications" $ do
    check
      [r|
      let f = fn(c, x, y) {
        if c { x } else { y }
      };
      let main = f(true, 5, 10);
    |]
      `shouldBe` Right (pure TypeInt)

  it "can type check used items" $ do
    check
      [r|
      mod A {
        pub let x = 5;
      }
      use A;
      let main = x;
    |]
      `shouldBe` Right (pure TypeInt)

  it "errors on undefined variable" $ do
    check
      [r|
      let main = x;
    |]
      `shouldSatisfy` isTypeError

  it "errors on if with branches of different types" $ do
    check
      [r|
      let main = if true { 5 } else { "hello" };
    |]
      `shouldSatisfy` isTypeError

  it "errors on if with non-bool condition" $ do
    check
      [r|
      let main = if 1 { 5 } else { 10 };
    |]
      `shouldSatisfy` isTypeError

  it "gets types from std" $ do
    check
      [r|
      use std;
      let main = add(1, 2);
    |]
      `shouldBe` Right (pure TypeInt)

    check
      [r|
      use std;
      let main = concat("hello", concat(" ", " world"));
    |]
      `shouldBe` Right (pure TypeString)

  it "checks input for built-ins" $ do
    check
      [r|
      use std;
      let main = add("hello", 4);
    |]
      `shouldSatisfy` isTypeError

  it "respects let type annotations" $ do
    check
      [r|
      let main: Int = "hello";
    |]
      `shouldSatisfy` isTypeError

    check
      [r|
      let main: String = "hello";
    |]
      `shouldBe` Right (pure TypeString)

  it "cannot assign mono to poly" $ do
    check
      [r|
      let main: a = "hello";
    |]
      `shouldSatisfy` isTypeError

  it "can assign to bound poly" $ do
    check
      [r|
      let f = fn(x: a) {
        let y = x;
        y
      };
      let main = f(5);
    |]
      `shouldBe` Right (pure TypeInt)

  it "can assign to explicit bound poly" $ do
    check
      [r|
      let f = fn(x: a) {
        let y: a = x;
        y
      };
      let main = f(5);
    |]
      `shouldBe` Right (pure TypeInt)

  it "cannot use poly as specific type" $ do
    check
      [r|
      let f = fn(x: a) {
        add(x, x)
      };
      let main = f(2);
    |]
      `shouldSatisfy` isTypeError

  -- TODO: effect row of argument should be empty
  it "can infer id applied to id" $ do
    check
      [r|
      let id = fn(x) { x };
      let main = id(id);
    |]
      `shouldSatisfy` \case
        Right (CompType _ (TypeArrow (Arrow [CompType _ (TypeV a)] (CompType _ (TypeV b))))) | a == b -> True
        _ -> False

  it "uses the function body to infer types" $ do
    check
      [r|
      let f = fn(x) {
        add(x, x)
      };
      let main = f("hello");
    |]
      `shouldSatisfy` isTypeError

  it "respects function type annotations" $ do
    check
      [r|
      let f = fn(x: Int) Int {
        x
      };

      let main = f(5);
    |]
      `shouldBe` Right (pure TypeInt)

    check
      [r|
      let f = fn(x: Int) Int {
        x
      };

      let main = f("hello");
    |]
      `shouldSatisfy` isTypeError

    check
      [r|
      let f = fn(x) Int {
        x
      };

      let main = f("hello");
    |]
      `shouldSatisfy` isTypeError

  it "checks function types" $ do
    check
      [r|
      let f: fn(Int) Int = fn(x) { x };
      let main = f(5);
    |]
      `shouldBe` Right (pure TypeInt)

    check
      [r|
      let f: fn(Int) Int = fn(x) { x };
      let main = f("hello");
    |]
      `shouldSatisfy` isTypeError

  it "checks first class functions" $ do
    check
      [r|
      use std;
      let f = fn(x) fn(Int) Int {
        fn(y) {
          add(x, y)
        }
      };
      let main = f(1)(2);
    |]
      `shouldBe` Right (pure TypeInt)

    check
      [r|
      use std;
      let f = fn(x) fn(Int) Int {
        fn(y) {
          add(x, y)
        }
      };
      let main = f("hello")(2);
    |]
      `shouldSatisfy` isTypeError

    check
      [r|
      use std;
      let f = fn(x) fn(Int) Int {
        fn(y) {
          add(x, y)
        }
      };
      let main = f(2)("hello");
    |]
      `shouldSatisfy` isTypeError

  it "can do polymorphic functions" $ do
    check
      [r|
      let f = fn(x) {
        x
      };
      let main = {
        let a = f(2);
        f("hello")
      };
    |]
      `shouldBe` Right (pure TypeString)

  it "can call effectless functions in main" $ do
    check
      [r|
      let f = fn(x: a) <> a {
        x
      };
      let main = f(5);
    |]
      `shouldBe` Right (pure TypeInt)

  it "cannot call effectful functions in main" $ do
    check
      [r|
      let f = fn(x: a) <A> a {
        x
      };
      let main = f(5);
    |]
      `shouldSatisfy` isTypeError

  it "can infer handler types" $ do
    check
      [r|
      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { x }
        bar(x) { resume(x) }
      };
    |]
      `shouldSatisfy` \case
        Right (CompType _ (TypeHandler (Effect [Ident "Foo" _] _) _ _)) -> True
        _ -> False

  it "can infer handler types with specific types in the operations" $ do
    check
      [r|
      use std;

      effect Foo {
        bar(Int) Int
      }

      let main = handler {
        return(x) { x }
        bar(x) { resume(add(x, x)) } 
      };
    |]
      `shouldSatisfy` \case
        Right (CompType _ (TypeHandler (Effect [Ident "Foo" _] _) _ _)) -> True
        _ -> False

  it "can cannot be more specific about types in handlers" $ do
    check
      [r|
      use std;

      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { x }
        bar(x) { add(x, x) } 
      };
    |]
      `shouldSatisfy` isTypeError

  it "handler return types must match" $ do
    check
      [r|
      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { "hello" }
        bar(x) { 5 }
      };
    |]
      `shouldSatisfy` isTypeError

    check
      [r|
      effect Foo {
        bar(a) a
      }

      let main = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };
    |]
      `shouldSatisfy` \case
        Right (CompType _ (TypeHandler (Effect [Ident "Foo" _] _) _ TypeString)) -> True
        _ -> False

  it "can apply a handler" $ do
    check
      [r|
      effect Foo {
        bar(a) a
      }

      let h = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };

      let main = handle[h] {
        5
      };
    |]
      `shouldBe` Right (pure TypeString)

  it "cannot use operation outside of handle" $ do
    check
      [r|
      effect Foo {
        bar(a) a
      }

      let main = bar(5);
    |]
      `shouldSatisfy` isTypeError

  it "cannot use operation in effectless function" $ do
    check
      [r|
      effect Foo {
        bar(a) a
      }
      
      let f = fn() <|e> Int {
        bar(5)
      };

      let h = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };

      let main = handle[h] { f() };
    |]
      `shouldSatisfy` isTypeError

    check
      [r|
      effect Foo {
        bar(a) a
      }
      
      let f = fn() <Foo|e> Int {
        bar(5)
      };

      let h = handler {
        return(x) { "hello" }
        bar(x) { "world" }
      };

      let main = handle[h] { f() };
    |]
      `shouldBe` Right (pure TypeString)

  it "can call function with effect in signature" $ do
    check
      [r|
      effect A {
        a() ()
      }

      let hf = handler {
        return(x) { x }
        a() { resume(()) }
      };

      let y = 5;

      let f = fn(x: Int) <A> Int {
        y
      };

      let main = handle[hf] f(5);
    |]
      `shouldBe` Right (pure TypeInt)

  it "type for resume must match return type of operation" $ do
    check
      [r|
      effect Foo {
        bar() Int
      }

      let h = handler {
        return(x) { x }
        bar() {
          resume(5)
        }
      };

      let main = handle[h] bar();
    |]
      `shouldBe` Right (pure TypeInt)

    check
      [r|
      effect Foo {
        bar() Int
      }

      let h = handler {
        return(x) { x }
        bar() {
          resume("hello")
        }
      };

      let main = handle[h] bar();
    |]
      `shouldSatisfy` isTypeError

  it "can use effects in function application" $ do
    check
      [r|
      use std;
      effect Foo {
        foo() Int
      }

      let hf = handler {
        return(x) { x }
        foo() { resume(2) }
      };

      let main = handle[hf] add(foo(), foo());
    |]
      `shouldBe` Right (pure TypeInt)

  it "can use effects in if" $ do
    check
      [r|
      use std;
      effect Foo {
        foo() Int
      }

      let hf = handler {
        return(x) { x }
        foo() { resume(2) }
      };

      let main = handle[hf] {
        if gt(foo(), 0) {
          foo()
        } else {
          sub(0, foo())
        }
      };
    |]
      `shouldBe` Right (pure TypeInt)

  it "accepts different orders of handlers" $ do
    check
      [r|
      use std;
      
      effect Foo {
        foo() Int
      }

      let hf = handler {
        return(x) { x }
        foo() { resume(2) }
      };

      effect Bar {
        bar() Int
      }

      let hb = handler {
        return(x) { x }
        bar() { resume(3) }
      };

      let main = add(
        { handle[hf] handle[hb] add(foo(), bar()) },
        { handle[hb] handle[hf] add(foo(), bar()) }
      );
    |]
      `shouldBe` Right (pure TypeInt)

  it "accepts different orders of handlers with explicitly typed function" $ do
    check
      [r|
      use std;
      
      effect Foo {
        foo() Int
      }

      let hf = handler {
        return(x) { x }
        foo() { resume(2) }
      };

      effect Bar {
        bar() Int
      }

      let hb = handler {
        return(x) { x }
        bar() { resume(3) }
      };

      let f = fn() <Foo,Bar> Int {
        add(foo(), bar())
      };

      let main = add(
        { handle[hf] handle[hb] f() },
        { handle[hb] handle[hf] f() }
      );
    |]
      `shouldBe` Right (pure TypeInt)

  it "can figure out higher-order effectful functions" $ do
    check
      [r|
      use std;

      effect Foo {
        foo() Int
      }

      let addFoo = fn(x: Int) <Foo> Int {
        add(foo(), x)
      };

      let applyTo2 = fn(f: fn(Int) <|e> Int) <|e> Int {
        f(2)
      };

      let main = applyTo2(addFoo);
    |]
      `shouldSatisfy` isTypeError

    check
      [r|
      use std;

      effect Foo {
        foo() Int
      }

      let addFoo = fn(x: Int) <Foo> Int {
        add(foo(), x)
      };

      let applyTo2 = fn(f: fn(Int) <|e> Int) <|e> Int {
        f(2)
      };

      let hf = handler {
        return(x) { x }
        foo() { resume(5) }
      };

      let main = handle[hf] applyTo2(addFoo);
    |]
      `shouldBe` Right (pure TypeInt)

  it "can infer an elaboration" $ do
    check
      [r|
      effect Foo! {
        foo!(Int) Int
      }

      effect Bar {
        bar() Int
      }

      let ef = elaboration Foo! -> <Bar> {
        foo!(x) {
          x
        }
      };

      let main = ef;
    |]
      `shouldSatisfy` \case
        Right (CompType _ (TypeElaboration (Effect [Ident "Foo!" _] _) row)) -> True
        _ -> False

  it "can infer explicit elab with empty row" $ do
    check
      [r|
      effect Foo! {
        foo!() Int
      }

      let ef = elaboration Foo! -> <> {
        foo!() { 5 }
      };

      let main = elab[ef] foo!();
    |]
      `shouldBe` Right (pure TypeInt)

  it "can infer explicit elab with another effect" $ do
    check
      [r|
      effect Foo! {
        foo!() Int
      }

      effect Bar {
        bar() Int
      }

      let hb = handler {
        return(x) { x }
        bar() { resume(6) }
      };

      let ef = elaboration Foo! -> <Bar> {
        foo!() { bar() }
      };

      let main = handle[hb] elab[ef] foo!();
    |]
      `shouldBe` Right (pure TypeInt)

    check
      [r|
      effect Foo! {
        foo!() Int
      }

      effect Bar {
        bar() Int
      }

      let hb = handler {
        return(x) { x }
        bar() { resume(6) }
      };

      let ef1 = elaboration Foo! -> <Bar> {
        foo!() { bar() }
      };

      let main = elab[ef] foo!();
    |]
      `shouldSatisfy` isTypeError

  it "elaborations must have the same type" $ do
    check
      [r|
      effect Foo! {
        foo!() Int
      }

      effect Bar {
        bar() Int
      }

      let hb = handler {
        return(x) { x }
        bar() { resume(6) }
      };

      let ef1 = elaboration Foo! -> <> {
        foo!() { 5 }
      };

      let ef2 = elaboration Foo! -> <Bar> {
        foo!() { bar() }
      };

      let main = handle[hb] elab[if true { ef1 } else { ef2 }] foo!();
    |]
      `shouldSatisfy` isTypeError

    check
      [r|
      effect Foo! {
        foo!() Int
      }

      effect Bar {
        bar() Int
      }

      let hb = handler {
        return(x) { x }
        bar() { resume(6) }
      };

      let ef1 = elaboration Foo! -> <Bar> {
        foo!() { 5 }
      };

      let ef2 = elaboration Foo! -> <Bar> {
        foo!() { bar() }
      };

      let main = handle[hb] elab[if true { ef1 } else { ef2 }] foo!();
    |]
      `shouldBe` Right (pure TypeInt)

  it "can do handle through elab" $ do
    check
      [r|
      use std;

      effect Foo! {
        foo!() Int
      }

      effect Bar {
        bar() Int
      }

      let ef = elaboration Foo! -> <> {
        foo!() { 5 }
      };

      let hb = handler {
        return(x) { x }
        bar() { resume(6) }
      };

      let main = handle[hb] elab[ef] add(foo!(), bar());
    |]
      `shouldBe` Right (pure TypeInt)

  it "does not accept a handler for elab" $ do
    check
      [r|
      effect Foo {
        foo() Int
      }

      let hf = handler {
        return(x) { x }
        foo() { resume(5) }
      };

      let main = elab[hf] foo();
    |]
      `shouldSatisfy` isTypeError

  it "does not get confused between effects from different modules" $ do
    check
      [r|
      mod A {
        effect Foo {
          foo() String
        }

        pub let hf = handler {
          return(x) { x }
          foo() { resume("handler in A") }
        };
      }
      
      use A;
      effect Foo {
        foo() Int
      }
      let main = handle[hf] foo();
    |]
      `shouldSatisfy` isTypeError

  it "can figure out implicit elab for one elaboration" $ do
    check
      [r|
      effect Foo! {
        foo!() Int
      }

      let ef = elaboration Foo! -> <> {
        foo!() { 5 }
      };

      let main = elab foo!();
    |]
      `shouldBe` Right (pure TypeInt)
  
  it "cannot figure out implicit elab for two elaborations" $ do
    check
      [r|
      effect Val! {
        val!() Int
      }

      let eVal1 = elaboration Val! -> <> {
        val!() { 1 }
      };
      
      let eVal2 = elaboration Val! -> <> {
        val!() { 2 }
      };

      let main = elab val!();
    |]
      `shouldSatisfy` isTypeError 

  it "number of function arguments should match" $ do
    check
      [r|
      let id = fn(x) { x };

      let call = fn(f: fn() a) a {
          f()
      };

      let main = call(id);
    |]
      `shouldSatisfy` isTypeError

testUnifyRows :: SpecWith ()
testUnifyRows = describe "unifyRows" $ do
  it "should unify rows with the same effects" $ do
    let e1 = ExplicitVar $ Ident "1" LocNone
    let e2 = ExplicitVar $ Ident "2" LocNone
    let foo = Ident "Foo" LocNone
    let res = runInfer $ unifyRows (rowOpen [Effect [foo] Map.empty] e1) (rowOpen [Effect [foo] Map.empty] e2)
    second fst res `shouldBe` Right ()