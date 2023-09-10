module Elaine.Std (stdBindings, stdTypes) where

import Data.Map (Map, fromList)
import Elaine.AST
  ( BuiltIn (..),
    Value (Bool, Constant, Int, String),
  )
import Elaine.Ident (Ident (Ident), Location (LocBuiltIn))
import Elaine.TypeVar (TypeVar (ExplicitVar))
import Elaine.Types (Arrow (Arrow), CompType (CompType), TypeScheme (TypeScheme), ValType (TypeArrow, TypeBool, TypeInt, TypeString), rowEmpty, rowVar)

arrow :: [ValType] -> ValType -> TypeScheme
arrow args ret =
  let a = ExplicitVar (Ident "a" LocBuiltIn)
      b = ExplicitVar (Ident "b" LocBuiltIn)
   in TypeScheme [] [a, b] $
        CompType (rowVar a) $
          TypeArrow $
            Arrow
              (map (CompType rowEmpty) args)
              (CompType (rowVar b) ret)

newBuiltIn :: String -> TypeScheme -> ([Value] -> Maybe Value) -> BuiltIn
newBuiltIn name t f = BuiltIn (Ident name LocBuiltIn) t $ \x -> case f x of
  Just a -> a
  Nothing -> error ("incorrect arguments for <" ++ name ++ ">")

stdBindings :: Map Ident Value
stdBindings =
  fromList $
    map
      (\b@(BuiltIn x _ _) -> (x, Constant b))
      allBuiltIns

stdTypes :: Map Ident TypeScheme
stdTypes = fromList $ map (\(BuiltIn x t _) -> (x, t)) allBuiltIns

allBuiltIns :: [BuiltIn]
allBuiltIns =
  [ bAdd,
    bSub,
    bNeg,
    bMul,
    bDiv,
    bModulo,
    bPow,
    bEq,
    bNeq,
    bGt,
    bLt,
    bGeq,
    bLeq,
    bNot,
    bAnd,
    bOr,
    bConcat,
    bShowInt,
    bShowBool
  ]

intBinOp :: String -> (Int -> Int -> Int) -> BuiltIn
intBinOp name op = newBuiltIn name (arrow [TypeInt, TypeInt] TypeInt) $ \case
  [Int x, Int y] -> Just $ Int $ op x y
  _ -> Nothing

intCmp :: String -> (Int -> Int -> Bool) -> BuiltIn
intCmp name op = newBuiltIn name (arrow [TypeInt, TypeInt] TypeBool) $ \case
  [Int x, Int y] -> Just $ Bool $ op x y
  _ -> Nothing

bAdd :: BuiltIn
bAdd = intBinOp "add" (+)

bSub :: BuiltIn
bSub = intBinOp "sub" (-)

bNeg :: BuiltIn
bNeg = newBuiltIn "neg" (arrow [TypeInt] TypeInt) $ \case
  [Int x] -> Just $ Int $ -x
  _ -> Nothing

bMul :: BuiltIn
bMul = intBinOp "mul" (*)

bDiv :: BuiltIn
bDiv = intBinOp "div" div

bModulo :: BuiltIn
bModulo = intBinOp "modulo" mod

bPow :: BuiltIn
bPow = intBinOp "pow" (^)

bEq :: BuiltIn
bEq = intCmp "eq" (==)

bNeq :: BuiltIn
bNeq = intCmp "neq" (/=)

bGt :: BuiltIn
bGt = intCmp "gt" (>)

bLt :: BuiltIn
bLt = intCmp "lt" (<)

bGeq :: BuiltIn
bGeq = intCmp "geq" (>=)

bLeq :: BuiltIn
bLeq = intCmp "leq" (<=)

bNot :: BuiltIn
bNot = newBuiltIn "not" (arrow [TypeBool] TypeBool) $ \case
  [Bool x] -> Just $ Bool $ not x
  _ -> Nothing

bAnd :: BuiltIn
bAnd = newBuiltIn "and" (arrow [TypeBool, TypeBool] TypeBool) $ \case
  [Bool x, Bool y] -> Just $ Bool $ x && y
  _ -> Nothing

bOr :: BuiltIn
bOr = newBuiltIn "or" (arrow [TypeBool, TypeBool] TypeBool) $ \case
  [Bool x, Bool y] -> Just $ Bool $ x || y
  _ -> Nothing

bConcat :: BuiltIn
bConcat = newBuiltIn "concat" (arrow [TypeString, TypeString] TypeString) $ \case
  [String x, String y] -> Just $ String $ x ++ y
  _ -> Nothing

bShowInt :: BuiltIn
bShowInt = newBuiltIn "show_int" (arrow [TypeInt] TypeString) $ \case
  [Int x] -> Just $ String $ show x
  _ -> Nothing

bShowBool :: BuiltIn
bShowBool = newBuiltIn "show_bool" (arrow [TypeBool] TypeString) $ \case
  [Bool x] -> Just $ String $ show x
  _ -> Nothing