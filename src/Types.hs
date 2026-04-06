{-# LANGUAGE LambdaCase, GADTs, DataKinds #-}


data ℕ = Zero | Succ ℕ
  deriving (Eq, Show)

-- the name Type is already taken for the type of types in Haskell
data Typ = Nat | TFun Typ Typ
  deriving (Eq, Show)

data Val τ where
  VNat :: ℕ -> Val Nat
  VFun :: (Val τ₁ -> Val τ₂) -> Val (TFun τ₁ τ₂)

data τ ∈ γ where
  Here :: τ ∈ (τ:γ)
  There :: τ ∈ γ -> τ ∈ (τ':γ)

data γ ⊢ τ where
  Var :: τ ∈ γ -> γ ⊢ τ
  Z :: γ ⊢ Nat
  S :: γ ⊢ Nat -> γ ⊢ Nat
  RecN :: γ ⊢ τ -> (Nat:τ:γ) ⊢ τ -> γ ⊢ Nat -> γ ⊢ τ
  Lam :: (τ₁:γ) ⊢ τ₂ -> γ ⊢ TFun τ₁ τ₂
  App :: γ ⊢ TFun τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂
  Let :: γ ⊢ τ₂ -> (τ₂:γ) ⊢ τ₂ -> γ ⊢ τ₂

type Env γ = forall τ. τ ∈ γ -> Val τ

empty :: Env '[]
empty = \case

cons :: Env γ -> Val τ -> Env (τ:γ)
cons e x Here = x
cons e x (There p) = e p

eval' :: '[] ⊢ τ -> Val τ
eval' e = eval empty e

eval :: Env γ -> γ ⊢ τ  -> Val τ
eval env = \case
  Z -> VNat Zero
  S e -> case eval env e of
    VNat n -> VNat (Succ n)
  Var x -> env x
  RecN z s Z -> eval env z
  RecN z s (S e) -> eval (cons (cons env (eval env (RecN z s e))) (eval env e)) s
  Lam e -> VFun (\v -> eval (cons env v) e)
  App f x -> case eval env f of
    VFun f' -> f' (eval env x)
  Let e₁ e₂ -> eval (cons env (eval env e₁)) e₂
