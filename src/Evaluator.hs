module Evaluator where

import Types
import Prelude hiding (sum)

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
step lam@(Lam {}) = Left $ Val lam
step (App fun arg) = Right $ stepFun fun arg
step prod@(Tup _) = Left $ Val prod
step (Proj idx prod) = Right $ stepProj idx prod
step inj@(Inj {}) = Left $ Val inj
step (Case sum cases) = pure $ stepCases sum cases

stepProj :: τ ∈ τs -> Term '[] ('TProd τs) -> Term '[] τ
stepProj idx (Tup tup) = lookupTuple idx tup
stepProj idx prod = case step prod of
  Left (Val prod') -> Proj idx prod'
  Right prod' -> Proj idx prod'

stepCases :: Term '[] ('TSum τs) -> Cases '[] τs τ -> Term '[] τ
stepCases (Inj Here term _) (CCons c _) = subst term c
stepCases (Inj (There i) term (SCons _ ts)) (CCons _ cs) =
  stepCases (Inj i term ts) cs
stepCases inj cases = case step inj of
  Left (Val inj') -> Case inj' cases
  Right inj' -> Case inj' cases

lookupTuple :: τ ∈ τs -> Tuple γ τs -> Term γ τ
lookupTuple Here (TCons term _) = term
lookupTuple (There i) (TCons _ rest) = lookupTuple i rest

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
substAll σ (Tup tup) = Tup (substTuple σ tup)
substAll σ (Proj i t) = Proj i (substAll σ t)
substAll σ (Inj i t s) = Inj i (substAll σ t) s
substAll σ (Case t cs) = Case (substAll σ t) (substCases σ cs)

substTuple :: Subst γ γ₀ -> Tuple γ₀ τs -> Tuple γ τs
substTuple _ TNil = TNil
substTuple σ (TCons t ts) = TCons (substAll σ t) (substTuple σ ts)

substCases :: Subst γ γ₀ -> Cases γ₀ τs τ -> Cases γ τs τ
substCases _ CNil = CNil
substCases σ (CCons t rest) = CCons (substAll (liftS σ) t) (substCases σ rest)

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
rename r (Tup t) = Tup (renameTuple r t)
rename r (Proj i e) = Proj i (rename r e)
rename r (Inj i t s) = Inj i (rename r t) s
rename r (Case t cs) = Case (rename r t) (renameCases r cs)

renameTuple :: Renaming γ γ₀ -> Tuple γ₀ τs -> Tuple γ τs
renameTuple _ TNil = TNil
renameTuple r (TCons t ts) = TCons (rename r t) (renameTuple r ts)

renameCases :: Renaming γ γ₀ -> Cases γ₀ τs τ -> Cases γ τs τ
renameCases _ CNil = CNil
renameCases r (CCons t rest) = CCons (rename (liftR r) t) (renameCases r rest)
