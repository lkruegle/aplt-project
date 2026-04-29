module Evaluator where

import Types
import BookKeeping

evaluate :: Exp -> Either String Val
evaluate = eval

eval :: Exp -> Either String Val
eval e = case toVal e of
  Just val -> Right val
  Nothing -> step e >>= eval

step :: Exp -> Either String Exp
step = _

toVal :: Exp -> Maybe Val
toVal (EFLam t e) = Just $ VLam t e
toVal (ETLam e) = Just $ VTLam e
toVal _ = Nothing
