module Evaluator where

import qualified Data.Map as Map

import Types(Env, Typ(..), Exp (..), Val(..))

eval' :: Exp -> Val
eval' e = eval Map.empty e

eval :: Env -> Exp -> Val
eval env (EVar x) = undefined
eval env (EFLam _ _ _) = undefined
eval env (EFApl _ _) = undefined
eval env (ETLam _ _) = undefined
eval env (ETApp _ _) = undefined
