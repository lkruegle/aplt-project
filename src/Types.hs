module Types where

data ℕ = Zero | Succ ℕ
  deriving (Eq, Show)

-- the name Type is already taken for the type of types in Haskell
data Typ = Nat | TFun Typ Typ
  deriving (Eq, Show)

data Val (τ :: Typ) where
  VNat :: ℕ -> Val Nat
  VFun :: (Val τ₁ -> Val τ₂) -> Val ('TFun τ₁ τ₂)

instance Show (Val τ) where
  show (VNat n) = show n
  show (VFun _) = "λ"

data (τ :: Typ) ∈ (γ :: [Typ]) where
  Here :: τ ∈ (τ:γ)
  There :: τ ∈ γ -> τ ∈ (τ':γ)

data (γ :: [Typ]) ⊢ (τ :: Typ) where
  Var :: τ ∈ γ -> γ ⊢ τ
  Z :: γ ⊢ 'Nat
  S :: γ ⊢ 'Nat -> γ ⊢ 'Nat
  RecN :: γ ⊢ τ -> ('Nat:τ:γ) ⊢ τ -> γ ⊢ 'Nat -> γ ⊢ τ
  Lam :: (τ₁:γ) ⊢ τ₂ -> γ ⊢ 'TFun τ₁ τ₂
  App :: γ ⊢ 'TFun τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂
  Let :: γ ⊢ τ₂ -> (τ₂:γ) ⊢ τ₂ -> γ ⊢ τ₂

type Env (γ :: [Typ]) = forall (τ :: Typ). τ ∈ γ -> Val τ

emptyEnv :: Env '[]
emptyEnv = \case

consEnv :: Env γ -> Val τ -> Env (τ:γ)
consEnv e x Here = x
consEnv e x (There p) = e p
