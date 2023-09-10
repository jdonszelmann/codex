module Hefty.Lambda.CBN where

import Free
import Hefty
import Elab
import Hefty.Lambda

instance (Functor f) => Elab' (Lambda (Free f) (Fun f)) f where
    elab = eLambdaCBN

eLambdaCBN :: forall f.
              Functor f
           => Elab (Lambda (Free f) (Fun f)) f
eLambdaCBN = Alg $ \case
  Lambda body   k -> k (Fun body)
  Var x         k -> x >>= k
  Apply fun arg k -> app fun arg >>= k
