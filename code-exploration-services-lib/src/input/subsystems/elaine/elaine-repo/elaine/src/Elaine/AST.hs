module Elaine.AST where

import Elaine.Ident (Ident (Ident))
import Elaine.TypeVar
import Elaine.Types (TypeScheme)

type Program = [Declaration]

data Visibility = Private | Public
  deriving (Show, Eq)

data Declaration = Declaration Visibility DeclarationType
  deriving (Show, Eq)

data DeclarationType
  = Use Ident
  | Module Ident [Declaration]
  | DecLet Ident (Maybe ASTComputationType) Expr
  | DecType Ident [Constructor]
  | DecEffect Ident [OperationSignature]
  deriving (Show, Eq)

data OperationSignature = OperationSignature Ident [ASTComputationType] ASTComputationType
  deriving (Show, Eq)

data Elaboration = Elaboration Ident Row [OperationClause]
  deriving (Show, Eq)

data Handler = Handler Function [OperationClause]
  deriving (Show, Eq)

data Function = Function [(Ident, Maybe ASTComputationType)] (Maybe ASTComputationType) Expr
  deriving (Show, Eq)

lam :: [Ident] -> Expr -> Value
lam a = Fn . Function (zip a (repeat Nothing)) Nothing

data Constructor = Constructor Ident [ASTComputationType]
  deriving (Show, Eq)

data OperationClause = OperationClause Ident [Ident] Expr
  deriving (Show, Eq)

clauseName :: OperationClause -> Ident
clauseName (OperationClause name _ _) = name

data Expr
  = App Expr [Expr]
  | If Expr Expr Expr
  | Handle Expr Expr
  | Match Expr [MatchArm]
  | -- The integer is mapped to a unique identifier while type checking
    ImplicitElab Int Expr
  | Elab Expr Expr
  | Var Ident
  | Let (Maybe Ident) (Maybe ASTComputationType) Expr Expr
  | Val Value
  deriving (Show, Eq)

data Value
  = Int Int
  | String String
  | Bool Bool
  | Fn Function
  | Hdl Handler
  | Elb Elaboration
  | Constant BuiltIn
  | Data Ident Ident [Expr]
  | Unit
  deriving (Show, Eq)

data BuiltIn = BuiltIn Ident TypeScheme ([Value] -> Value)

instance Show BuiltIn where
  show (BuiltIn (Ident x _) _ _) = "<built-in " ++ x ++ ">"

instance Eq BuiltIn where
  (BuiltIn x _ _) == (BuiltIn y _ _) = x == y

-- A match is as simple as possible:
--  - Only constructors can be matches
--  - All constructors must be present
data MatchArm = MatchArm Pattern Expr
  deriving (Show, Eq)

-- A pattern consisting of a constructor identifier and
-- and a list of variables to bind.
data Pattern = Pattern Ident [Ident]
  deriving (Show, Eq)

data Row = Row [Ident] (Maybe Ident)
  deriving (Show, Eq)

data ASTComputationType = ASTComputationType Row ASTValueType
  deriving (Show, Eq)

data ASTValueType
  = TypeName Ident
  | TypeUnit
  | TypeArrow [ASTComputationType] ASTComputationType
  | TypeHandler Ident TypeVar ASTValueType
  | TypeElaboration Ident Row
  deriving (Show, Eq)
