module Evaluator where

import Types

eval' :: '[] ⊢ τ -> Val τ
eval' e = eval emptyEnv e

eval :: Env γ -> γ ⊢ τ  -> Val τ
eval env = \case
  Z -> VNat Zero
  S e -> case eval env e of
    VNat n -> VNat (Succ n)
  Var x -> env x
  RecN z s Z -> eval env z
  RecN z s (S e) -> eval (consEnv (consEnv env (eval env (RecN z s e))) (eval env e)) s
  Lam e -> VFun (\v -> eval (consEnv env v) e)
  App f x -> case eval env f of
    VFun f' -> f' (eval env x)
  Let e₁ e₂ -> eval (consEnv env (eval env e₁)) e₂
