module Elaine.Ident (Ident (Ident, idText), Location (LocOffset, LocBuiltIn, LocNone)) where

import Data.Aeson (ToJSON)
import GHC.Generics

data Ident = Ident
  { idText :: String,
    location :: Location
  }
  deriving (Show, Generic)

instance ToJSON Ident

data Location = LocOffset Int | LocBuiltIn | LocNone
  deriving (Show, Generic)

instance ToJSON Location

instance Ord Ident where
  a <= b = idText a <= idText b

instance Eq Ident where
  a == b = idText a == idText b
