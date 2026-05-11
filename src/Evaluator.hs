module Evaluator where

import Types

-- | Evaluator entrypoint, convert an expression to a Value
evaluate :: '[] ⊢ τ -> Val τ
evaluate term = case step term of
  Right term' -> evaluate term'
  Left val -> val

-- | Perform the next reduction of the given term, if one exists
step :: '[] ⊢ τ -> Either (Val τ) ('[] ⊢ τ)
step (Var x) = absurdVar x
step Zero = Left $ VNat Zero
step (Succ e) = Left $ VNat (Succ e)
step (Lam arg body) = Left $ VLam arg body
step (App fun arg) = Right $ case fun of
  (Lam _ body) -> subst arg body
  app@(App _ _) -> case step app of
    Right fun' -> App fun' arg
    Left (VLam st body') -> App (Lam st body') arg
  (Var x) -> absurdVar x

-- | START: Substitution Utilities

-- | Perform a substitution, replacing the first bound variable with the given
-- term of the same type.
subst :: γ ⊢ τ' -> (τ' : γ) ⊢ τ -> γ ⊢ τ
subst e' = substAll $ \case
  Here -> e'
  There x -> Var x

type Subst γ γ₀ = forall τ. τ ∈ γ₀ -> γ ⊢ τ

liftS :: Subst γ γ₀ -> Subst (τ ': γ) (τ ': γ₀)
liftS _ Here = Var Here
liftS σ (There x) = weaken (σ x)

substAll :: Subst γ γ₀ -> γ₀ ⊢ τ -> γ ⊢ τ
substAll σ (Var x) = σ x
substAll _ Zero = Zero
substAll σ (Succ e) = Succ (substAll σ e)
substAll σ (App f a) = App (substAll σ f) (substAll σ a)
substAll σ (Lam τ e) = Lam τ (substAll (liftS σ) e)

type Renaming γ γ₀ = forall τ. τ ∈ γ₀ -> τ ∈ γ

weaken :: γ ⊢ τ -> (τ' : γ) ⊢ τ
weaken = rename There

liftR :: Renaming γ γ₀ -> Renaming (τ ': γ) (τ ': γ₀)
liftR _ Here = Here
liftR r (There x) = There (r x)

rename :: Renaming γ γ₀ -> γ₀ ⊢ τ -> γ ⊢ τ
rename r (Var x) = Var (r x)
rename _ Zero = Zero
rename r (Succ e) = Succ (rename r e)
rename r (App e1 e2) = App (rename r e1) (rename r e2)
rename r (Lam s e) = Lam s (rename (liftR r) e)
