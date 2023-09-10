{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Effen where

import Control.Monad
import Data.Set (Set)
import qualified Data.Map as Map
import AST

-- Small-step semantics
-- Just means that the computation was updated
-- Nothing means that the computation is stuck
step :: Comp -> Maybe Comp

step (Do x (Return v) f)
  = Just $ case x of 
      Just x -> substituteC x v f
      Nothing -> f

step (Do x c f)
  | Just c' <- step c
  = Just $ Do x c' f

step (Do x (Op op v f1) f2)
  = Just $ Op op v (Do x f1 f2)

step (Do x (Oph op v f1) f2)
  = Just $ Oph op v (Do x f1 f2)

step (If (ValBool True) c _) = Just c
step (If (ValBool False) _ c) = Just c

step (App (ValFun xs f) vs) = Just $ substitute (zip xs vs) f
step (App (ValBuiltIn bi) v) = Just $ builtInComp bi v

step (Handle h c)
  | Just c' <- step c
  = Just $ Handle h c'

step (Handle (ValHan _ (x, ret) _) (Return v))
  = Just $ substituteC x v ret

step (Handle (ValHan eff ret ops) (Op op vs c_op))
  | Just (HandleOp xs comp) <- Map.lookup op ops
  = Just $ substituteC "$k" (ValFun ["$y"] (Handle (ValHan eff ret ops) c_op))
      (substitute (zip xs vs) comp)

step (Handle (ValHan eff ret ops) (Op op v c_op))
  = Just $ Op op v (Handle (ValHan eff ret ops) c_op)

step (Handle h (Oph op c_in c_op))
  = Just $ Oph op c_in (Handle h c_op)

-- step (Elaborate e c)
--   | Just c' <- step c
--   = Just $ Elaborate e c'

-- step (Elaborate _ (Return v)) = Just $ Return v

-- step (Elaborate (ValElab eff ops) (Oph op cs c_op))
--   | Just (HandleOp xs comp) <- Map.lookup op ops
--   = Just $ substitute (zip xs args) (Elaborate (ValElab eff op_map) (c_op x))
--   where
--     args = map (Elaborate (ValElab eff ops)) cs

-- step (Elaborate (ValElab _ map) (Oph op c_in c_op))
--   = Just $ Oph op c_in (Elaborate (Elab map) . c_op)

-- step (Elaborate e (Op op v c_op))
--   = Just $ Op op v (Elaborate e . c_op)

step x = Nothing

-- Step until stuck
multistep :: (a -> Maybe a) -> a -> a
multistep step c = maybe c (multistep step) (step c)

multistep' :: Show a => (a -> Maybe a) -> a -> IO a
multistep' step c = case step c of
  Just new_c -> do
    putStrLn ("Step: " ++ show new_c)
    multistep' step new_c
  Nothing -> pure c

-- Evaluate and get the final value (might fail)
eval :: Comp -> Val
eval c = case multistep step c of
  Return v -> v
  c -> error ("Evaluation got stuck at " ++ show c)

eval' :: Comp -> IO Val
eval' c = do
  steps <- multistep' step c
  case steps of
    Return v -> pure v
    c -> error ("Evaluation got stuck at " ++ show c)

-- Utils

-- Variable substitution:
-- TODO: Make sure that variable shadowing doesn't mess things up
substitute :: [(String, Val)] -> Comp -> Comp
substitute subs c = foldr (\(x,v) c -> substituteC x v c) c subs

substituteC :: String -> Val -> Comp -> Comp
substituteC x v = \case
  Return v'    -> Return (subV v')
  Op op v c    -> Op op (map subV v) (subC c)
  Do s c1 c2   -> Do s (subC c1) (if s == Just x then c2 else subC c2)
  If v' c1 c2  -> If (subV v') (subC c1) (subC c2)
  App v vs     -> App (subV v) (map subV vs)
  Handle v' c  -> Handle (subV v') (subC c)
  where 
    subV = substituteV x v
    subC = substituteC x v

substituteV :: String -> Val -> Val -> Val
substituteV x v = \case
  ValFun args c
    | notElem x args
    -> ValFun args (subC c)
  ValVar var
    | x == var
    -> v
  ValTuple as -> ValTuple (map subV as)
  val -> val
  where
    subC = substituteC x v
    subV = substituteV x v
