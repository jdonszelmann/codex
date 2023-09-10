module Hefty.Lambda.CBV where

import Free
import Hefty
import Elab
import Hefty.Lambda

instance (Functor f) => Elab' (Lambda Id (Fun f)) f where
    elab = eLambdaCBV

eLambdaCBV :: forall f.
              Functor f
           => Elab (Lambda Id (Fun f)) f
eLambdaCBV = Alg $ \case
  Lambda body   k -> k (Fun body)
  Var x         k -> k (unId x)
  Apply fun arg k -> do
    v <- arg
    app fun (Id v) >>= k
