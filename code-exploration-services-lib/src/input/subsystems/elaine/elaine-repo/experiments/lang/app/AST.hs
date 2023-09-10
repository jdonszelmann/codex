module AST where

import Data.Set
import Data.Map


type Op = String

----------------
-- Types
----------------
data TypeVal
  = TypeBool
  | TypeStr
  | TypeInt
  | TypeUnit
  | TypeTuple [TypeVal]
  | TypeFun [TypeVal] TypeComp
  | TypeHan Ident TypeVal TypeVal
  deriving (Show, Eq)

data TypeComp = TypeComp TypeVal (Set Op)
  deriving (Show, Eq)

data TypeOp = TypeOp [TypeVal] TypeVal
  deriving (Show, Eq)

----------------
-- Values
----------------
data HandleOp = HandleOp [Ident] Comp
  deriving (Show, Eq)

data Val
  = ValUnit
  | ValVar Ident
  | ValStr String
  | ValInt Int
  | ValBool Bool
  | ValTuple [Val]
  | ValFun [Ident] Comp
  | ValHan Ident (String, Comp) (Map Op HandleOp)
  | ValElab Ident (Map Op HandleOp)
  | ValBuiltIn BuiltIn
  deriving (Show, Eq)

data BuiltIn = BuiltIn {
  builtInName :: String,
  -- builtInType :: ([TypeVal], TypeComp),
  builtInComp :: [Val] -> Comp
}

data TypedVar = TypedVar Ident TypeVal
  deriving (Show, Eq)

instance Show BuiltIn where
  show bi = "<builtin " ++ show (builtInName bi) ++ ">"

instance Eq BuiltIn where
  a == b = builtInName a == builtInName b

----------------
-- Computation
----------------
data Comp
  = Return Val
  | Op Op [Val] Comp
  | Oph Op [Comp] Comp
  | Do (Maybe Ident) Comp Comp
  | If Val Comp Comp
  | App Val [Val]
  | Handle Val Comp
  | Elaborate Val Comp
  deriving (Show, Eq)

----------------
-- Declarations
----------------
data Decl
  -- ident, arguments, return type, computation
  = DeclFun String [TypedVar] TypeComp Comp
  -- ident, operations
  | DeclEffect String [Operation]
  deriving (Show, Eq)

data Operation = Operation Ident TypeOp
  deriving (Show, Eq)