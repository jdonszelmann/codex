module Elaine.Transform where

import Data.List (isSuffixOf)
import Data.Map (Map, (!))
import Elaine.AST
import Elaine.Eval (subst)
import Elaine.Ident (Ident (Ident, idText), Location (LocNone))

elabToHandle :: Program -> Program
elabToHandle = foldProgram elabToHandle'

elabToHandle' :: Expr -> Expr
elabToHandle' = \case
  Elab e1 e2 -> Handle e1 e2
  App (Var x) args
    | isHigherOrder x ->
        App (App (Var x) (map (Val . lam []) args)) []
  Val (Elb (Elaboration _ _ clauses)) ->
    let mapping params = zip params (map (\p -> App (Var p) []) params)
        x = Ident "x" LocNone
        resume = Ident "resume" LocNone
        f' (OperationClause x' params e) = OperationClause x' params (App (Var resume) [Val $ lam [] (subst (mapping params) e)])
        clauses' = map f' clauses
     in Val $
          Hdl $
            Handler
              (Function [(x, Nothing)] Nothing (Var x))
              clauses'
  e -> e

makeElabExplicit :: Map Int [Ident] -> Program -> Program
makeElabExplicit m = foldProgram (makeElabExplicit' m)

makeElabExplicit' :: (Map Int [Ident]) -> Expr -> Expr
makeElabExplicit' elabIdents = \case
  ImplicitElab i e -> foldr (\f x -> f x) e (map (Elab . Var) $ elabIdents ! i)
  e -> e

foldProgram :: (Expr -> Expr) -> Program -> Program
foldProgram f = map (foldDec f)

foldDec :: (Expr -> Expr) -> Declaration -> Declaration
foldDec f (Declaration vis decType) = Declaration vis $ case decType of
  DecLet x t e -> DecLet x t (foldExpr f e)
  Module x decs -> Module x (foldProgram f decs)
  x -> x

foldExpr :: (Expr -> Expr) -> Expr -> Expr
foldExpr f' e = f' (foldInner e)
  where
    foldInner = \case
      Elab e1 e2 -> Elab (f e1) (f e2)
      ImplicitElab i e1 -> ImplicitElab i (f e1)
      App e1 args -> App (f e1) (map f args)
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      Handle e1 e2 -> Handle (f e1) (f e2)
      Match e1 arms -> Match (f e1) (map (\(MatchArm p e') -> MatchArm p (f e')) arms)
      Var x -> Var x
      Let x t e1 e2 -> Let x t (f e1) (f e2)
      Val v -> Val $ foldVal f' v
      where
        f = foldExpr f'

foldVal :: (Expr -> Expr) -> Value -> Value
foldVal f = \case
  Fn (Function params ret body) ->
    Fn $ Function params ret (foldExpr f body)
  Hdl (Handler (Function params ret body) clauses) ->
    Hdl $
      Handler
        (Function params ret (foldExpr f body))
        (foldClauses clauses)
  Elb (Elaboration from to clauses) ->
    Elb $
      Elaboration from to (foldClauses clauses)
  x -> x
  where
    foldClauses = map (\(OperationClause x params e) -> OperationClause x params (foldExpr f e))

isAlgebraic :: Ident -> Bool
isAlgebraic = not . isHigherOrder

isHigherOrder :: Ident -> Bool
isHigherOrder x = "!" `isSuffixOf` idText x