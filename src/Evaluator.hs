module Evaluator where

import Types
import BookKeeping

-- | Evaluator entrypoint, convert an expression to a Value
evaluate :: Exp -> Val
evaluate e = case toVal e of
  Just val -> val
  Nothing -> evaluate . step $ e

-- | Check if an expression is a Value.
toVal :: Exp -> Maybe Val
-- 16.3a
toVal (EFLam t e) = Just $ VLam t e
-- 16.3b
toVal (ETLam e) = Just $ VTLam e
toVal _ = Nothing

-- | Implement the step-wise evaluation of an expression
--
-- Uses a Lazy evaluation strategy
step :: Exp -> Exp
-- 16.3c
step (EFApp (EFLam _ body) arg) = substExp arg body
-- 16.3d
step (EFApp e arg) = EFApp (step e) arg
-- 16.3f
step (ETApp (ETLam body) t) = substTypInExp t body
-- 16.3g
step (ETApp e t) = ETApp (step e) t
step e =
  error $ "Given expression " <> show e <> " has no valid step-wise dynamics."
