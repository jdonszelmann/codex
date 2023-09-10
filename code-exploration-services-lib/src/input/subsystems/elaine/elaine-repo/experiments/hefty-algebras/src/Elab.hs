module Elab where

import Free
import Hefty

type Elab h f = Alg h (Free f)

-- Implicit elaboration using typeclass
class (HFunctor h, Functor f) => Elab' h f where
    elab :: Alg h (Free f)

-- Lift needs an implementation
instance (Functor f, Functor g, f < g) => Elab' (Lift f) g where 
    elab = eLift

-- Hopefully this just works
instance (Elab' h1 f, Elab' h2 f) => Elab' (h1 ⊕ h2) f where
    elab = elab ⊕ elab

newtype Lift f (h :: * -> *) k = Lift (f k)
  deriving Functor

instance Functor f => HFunctor (Lift f) where
  hmap _ (Lift x) = Lift x

eLift :: ( f < g ) => Elab (Lift f) g
eLift = Alg $ \case
  Lift x -> Do $ inj x

lift0 :: forall f h.
         ( HFunctor h
         , Lift f <| h )
      => (Hefty h () -> f (Hefty h ())) -> Hefty h ()
lift0 f = Op $ injH $ Lift $ f (return ())

lift :: ( HFunctor h
        , Lift f <| h )
     => ((a -> Hefty h a) -> f (Hefty h a)) -> Hefty h a
lift f = Op $ injH $ Lift $ f return
