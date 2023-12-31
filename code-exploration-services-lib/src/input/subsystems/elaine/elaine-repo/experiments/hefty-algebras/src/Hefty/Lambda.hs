module Hefty.Lambda where

import Hefty
import Free
import Elab

data Lambda c fun f k
  = forall t1 t2. Lambda (c t1 -> f t2)        (fun (c t1) t2 -> k)
  | forall t.     Var   (c t)                  (t             -> k)
  | forall t1 t2. Apply (fun (c t1) t2) (f t1) (t2            -> k)

deriving instance forall c fun f. Functor (Lambda c fun f)

instance HFunctor (Lambda c fun) where
  hmap f (Lambda body   k) = Lambda (f . body) k
  hmap _ (Var x         k) = Var x k
  hmap f (Apply fun arg k) = Apply fun (f arg) k

lambda :: forall fun c h t1 t2.
          Lambda c fun <| h
       => (c t1 -> Hefty h t2)
       -> Hefty h (fun (c t1) t2)
lambda body = Op $ injH $ Lambda body Return

var :: forall fun c h t.
       Lambda c fun <| h
    => c t -> Hefty h t
var x = Op $ injH @(Lambda c fun) $ Var x Return

apply :: forall fun c h t1 t2.
         Lambda c fun <| h
      => fun (c t1) t2 -> Hefty h t1 -> Hefty h t2
apply fun arg = Op $ injH $ Apply fun arg Return

newtype Fun f t1 t2 = Fun { app :: t1 -> Free f t2 }           

