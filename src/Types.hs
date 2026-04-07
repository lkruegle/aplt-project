module Types where

import Kx.Abs (Ident)

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

data STyp (τ :: Typ) where
  SNat :: STyp 'Nat
  SFun :: STyp τ₁ -> STyp τ₂ -> STyp ('TFun τ₁ τ₂)

instance Show (STyp τ) where
  show SNat = show Nat
  show (SFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

data Ctx (γ :: [Typ]) where
  EmptyC :: Ctx '[]
  ConsC :: Ident -> STyp τ -> Ctx γ -> Ctx (τ:γ)

data Found (γ :: [Typ]) where
  Found :: STyp τ -> τ ∈ γ -> Found γ

lookupCtx :: Ident -> Ctx γ -> Maybe (Found γ)
lookupCtx _ EmptyC = Nothing
lookupCtx n (ConsC n' typ ctx)
  | n == n' = Just $ Found typ Here
  | otherwise = case lookupCtx n ctx of
    Nothing -> Nothing
    (Just (Found typ' ctx')) -> Just (Found typ' (There ctx'))

data Inferred (γ :: [Typ]) where
  Inferred :: STyp τ -> γ ⊢ τ -> Inferred γ
