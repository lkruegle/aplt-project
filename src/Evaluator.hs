module Evaluator where

import Types
import BookKeeping

evaluate :: Exp -> Val
evaluate e = case toVal e of
  Just val -> val
  Nothing -> evaluate . step $ e

toVal :: Exp -> Maybe Val
-- 16.3a
toVal (EFLam t e) = Just $ VLam t e
-- 16.3b
toVal (ETLam e) = Just $ VTLam e
toVal _ = Nothing

-- LAZY EVALUATION STRAT
step :: Exp -> Exp
-- 16.3c LAZY ONLY
step (EFApp (EFLam _ body) arg) = substExp arg body
-- 16.3d
step (EFApp e arg) = EFApp (step e) arg
-- 16.3f
step (ETApp (ETLam body) t) = substTypInExp t body
-- 16.3g
step (ETApp e t) = ETApp (step e) t
step e =
  error $ "Given expression " <> show e <> " has no valid step-wise dynamics."
