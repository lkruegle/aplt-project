module Evaluator where

import Types

-- | Evaluator entrypoint, convert an expression to a Value
evaluate :: '[] ⊢ τ -> Val τ
evaluate term = case step term of
  Right term' -> evaluate term'
  Left val -> val

-- | Perform the next reduction of the given term, if one exists
step :: '[] ⊢ τ -> Either (Val τ) ('[] ⊢ τ)
step (App fun arg) = Right $ case fun of
  (Lam _ body) -> subst arg body
step Zero = Left $ VNat Zero
step (Succ e) = Left $ VNat (Succ e)
step (Lam arg body) = Left $ VLam arg body
step (Var x) = case x of {} -- Impossible at type level, x cannot be a member of '[]

subst :: γ ⊢ τ' -> (τ' : γ) ⊢ τ -> γ ⊢ τ
subst e' = substAll $ \case
  Here -> e'
  There x -> Var x

type Subst γ γ₀ = forall τ. τ ∈ γ₀ -> γ ⊢ τ

substAll :: Subst γ γ₀ -> γ₀ ⊢ τ -> γ ⊢ τ
substAll σ (Var x) = σ x
substAll _ Zero = Zero
substAll σ (Succ e) = Succ (substAll σ e)
substAll σ (App f a) = App (substAll σ f) (substAll σ a)
-- substAll σ (Lam τ e) = Lam τ (subst (lift σ) e)
