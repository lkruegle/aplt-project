module Evaluator where

import BookKeeping
import Types

-- | Evaluator entrypoint, convert an expression to a Value
evaluate :: Exp -> Val
evaluate e = case toVal e of
  Just val -> val
  Nothing -> evaluate . step $ e

-- | Check if an expression is a Value.
toVal :: Exp -> Maybe Val
-- 16.3a
toVal e@(EFLam _ _) = Just $ Val e
-- 16.3b
toVal e@(ETLam _) = Just $ Val e
-- 9.2a
toVal e@EZero = Just $ Val e
-- 9.2b
toVal e@(ESucc e') = toVal e' *> Just (Val e)
-- 10.4a --special case of rule that are technically eager
toVal e@(ETupl (i:is)) = toVal (ETupl is) *> toVal i *> Just (Val e)
toVal e@(ETupl []) = Just $ Val e
-- 11.4a
toVal e@(EInj _ c) = toVal c *> Just (Val e)
toVal _ = Nothing

-- | Implement the step-wise evaluation of an expression
--
-- Uses a Lazy evaluation strategy
step :: Exp -> Exp
-- 9.3a
step (ESucc e) = ESucc $ step e
-- 16.3c/e
step (EFApp f@(EFLam _ body) arg) = case toVal arg of
  Just (Val _) -> substExp arg body -- 16.3c
  Nothing -> EFApp f (step arg) -- 16.3e
-- 16.3d
step (EFApp e arg) = EFApp (step e) arg
-- 16.3f
step (ETApp (ETLam body) t) = substTypInExp t body
-- 16.3g
step (ETApp e t) = ETApp (step e) t
-- 10.4b
step (ETupl tup) = ETupl $ map go tup
  where
    go e = case toVal e of
      Nothing -> step e
      Just _ -> e
-- 10.4c and 10.4d
step (EProj e i) = case toVal e of
  Just (Val (ETupl es)) -> es !! i -- 10.4d
  _ -> EProj (step e) i -- 10.4c
-- 11.4b
step (EInj i e) = EInj i $ step e
-- 11.4c/d
step (ECase e es) = case e of
  EInj i e' -> substExp e' (es !! i) -- 11.4d
  _ -> ECase (step e) es --11.4c
-- dynamics of sums and products
step (EAnn e _) = step e -- EAnn is transparent during evaluation.
step e =
  error $ "Given expression " <> show e <> " has no valid step-wise dynamics."

showVal :: Val -> String
showVal (Val e)  = case expToInt e of
  Just n -> show n
  Nothing -> show e

expToInt :: Exp -> Maybe Int
expToInt e = case e of
  EZero -> Just 0
  ESucc e' -> do
    n <- expToInt e'
    Just (1 + n)
  _ -> Nothing
