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
-- 9.2a
toVal e@EZero = Just $ VNat (toInt e)
-- 9.2b
toVal e@(ESucc _) = Just $ VNat (toInt e)
-- 10.4a --special case of rule that are technically eager
toVal (ETupl es) = do
  vs <- mapM toVal es
  Just $ VProd vs

toVal (EInj i e) = VInj i <$> toVal e

toVal _ = Nothing

toInt :: Exp -> Int
toInt EZero = 0
-- ESucc must evaluate e here to handle lazy dynamics
toInt (ESucc e) = case evaluate e of
  VNat n -> 1 + n
  v -> error $ "Cannot convert value " <> show v <> " to Int."
toInt e = error $ "Cannot convert " <> show e <> " to Int."

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
--10.4c and 10.4d
step (EProj e i) = case e of
  ETupl es -> es !! i
  _ -> EProj (step e) i

-- dynamics of sums and products

step e =
  error $ "Given expression " <> show e <> " has no valid step-wise dynamics."
