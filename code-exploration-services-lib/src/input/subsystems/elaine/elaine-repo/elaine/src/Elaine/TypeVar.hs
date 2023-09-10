module Elaine.TypeVar where

import Elaine.Ident (Ident)

data TypeVar = ImplicitVar Int | ExplicitVar Ident
  deriving (Show, Eq, Ord)
