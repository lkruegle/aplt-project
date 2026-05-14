module Evaluator where

import Types

-- | Evaluator entrypoint, convert an expression to a Value
evaluate :: Term '[] τ -> Val τ
evaluate term = case step term of
  Right term' -> evaluate term'
  Left val -> val

-- | Perform the next reduction of the given term, if one exists
step :: Term '[] τ -> Either (Val τ) (Term '[] τ)
step (Var x) = absurdVar x
step Zero = Left $ Val Zero
step (Succ e) = Left $ Val (Succ e)
step lam@(Lam _ _) = Left $ Val lam
step (App fun arg) = Right $ stepFun fun arg
step prod@(Prod _) = Left $ Val prod
step (Proj idx prod) = Right $ stepProj idx prod

stepProj :: τ ∈ τs -> Term '[] ('TProd τs) -> Term '[] τ
stepProj idx (Prod tup) = lookupTuple idx tup
stepProj idx prod = case step prod of
  Left (Val prod') -> Proj idx prod'
  Right prod' -> Proj idx prod'

lookupTuple :: τ ∈ τs -> Tuple '[] τs -> Term '[] τ
lookupTuple Here (Cons term _) = term
lookupTuple (There i) (Cons _ rest) = lookupTuple i rest

stepFun :: Term '[] ('TArr τ₁ τ₂) -> Term '[] τ₁ -> Term '[] τ₂
stepFun (Lam _ body) arg = subst arg body
stepFun term arg = case step term of
  Right fun' -> App fun' arg
  Left (Val lam) -> App lam arg

-- | START: Substitution Utilities

-- | Perform a substitution, replacing the first bound variable with the given
-- term of the same type.
subst :: Term γ τ' -> Term (τ' : γ) τ -> Term γ τ
subst e' = substAll $ \case
  Here -> e'
  There x -> Var x

type Subst γ γ₀ = forall τ. τ ∈ γ₀ -> Term γ τ

liftS :: Subst γ γ₀ -> Subst (τ ': γ) (τ ': γ₀)
liftS _ Here = Var Here
liftS σ (There x) = weaken (σ x)

substAll :: Subst γ γ₀ -> Term γ₀ τ -> Term γ τ
substAll σ (Var x) = σ x
substAll _ Zero = Zero
substAll σ (Succ e) = Succ (substAll σ e)
substAll σ (App f a) = App (substAll σ f) (substAll σ a)
substAll σ (Lam τ e) = Lam τ (substAll (liftS σ) e)
substAll σ (Prod tup) = Prod (substTuple σ tup)
substAll σ (Proj i t) = Proj i (substAll σ t)

substTuple :: Subst γ γ₀ -> Tuple γ₀ τs -> Tuple γ τs
substTuple _ Unit = Unit
substTuple σ (Cons t ts) = Cons (substAll σ t) (substTuple σ ts)

type Renaming γ γ₀ = forall τ. τ ∈ γ₀ -> τ ∈ γ

weaken :: Term γ τ -> Term (τ' : γ) τ
weaken = rename There

liftR :: Renaming γ γ₀ -> Renaming (τ ': γ) (τ ': γ₀)
liftR _ Here = Here
liftR r (There x) = There (r x)

rename :: Renaming γ γ₀ -> Term γ₀ τ -> Term γ τ
rename r (Var x) = Var (r x)
rename _ Zero = Zero
rename r (Succ e) = Succ (rename r e)
rename r (App e1 e2) = App (rename r e1) (rename r e2)
rename r (Lam s e) = Lam s (rename (liftR r) e)
rename r (Prod t) = Prod (renameTuple r t)
rename r (Proj i e) = Proj i (rename r e)

renameTuple :: Renaming γ γ₀ -> Tuple γ₀ τs -> Tuple γ τs
renameTuple _ Unit = Unit
renameTuple r (Cons t ts) = Cons (rename r t) (renameTuple r ts)
