module Types (
  module Types,
  Ident(..)
) where
import Kx.Abs(Ident(..))



data (γ :: [Typ]) ⊢ (τ :: Typ) where
  Var :: τ ∈ γ -> γ ⊢ τ
  Zero :: γ ⊢ 'TNat
  Succ :: γ ⊢ TNat -> γ ⊢ TNat
  Lam :: STyp τ₁ -> (τ₁:γ) ⊢ τ₂ -> γ ⊢ 'TArr τ₁ τ₂
  App :: γ ⊢ 'TArr τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂
  -- TODO: Define the rest of the terms
  -- All :: ??
  -- Free :: ??

data (τ :: Typ) ∈ (γ :: [Typ]) where
  Here :: τ ∈ (τ:γ)
  There :: τ ∈ γ -> τ ∈ (τ':γ)

data Found (γ :: [Typ]) where
  Found :: STyp τ -> τ ∈ γ -> Found γ

type Env (γ :: [Typ]) = forall (τ :: Typ). τ ∈ γ -> Val τ

emptyEnv :: Env '[]
emptyEnv = \case

consEnv :: Env γ -> Val τ -> Env (τ:γ)
consEnv e x Here = x
consEnv e x (There p) = e p

data STyp (τ :: Typ) where

  SNat :: STyp 'TNat
  SArr :: STyp τ₁ -> STyp τ₂ -> STyp ('TArr τ₁ τ₂)
  -- TODO: Define the rest of the STyps

data SomeSTyp where
  SomeSTyp :: STyp τ -> SomeSTyp

toSTyp :: Typ -> SomeSTyp
toSTyp TNat = SomeSTyp SNat
toSTyp (TArr a r) = case (toSTyp a, toSTyp r) of
  (SomeSTyp sa, SomeSTyp sr) -> SomeSTyp $ SArr sa sr
toSTyp _ = undefined

instance Show (STyp τ) where
  show SNat = show TNat
  show (SArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

data Ctx (γ :: [Typ]) where
  EmptyC :: Ctx '[]
  ConsC :: Ident -> STyp τ -> Ctx γ -> Ctx (τ:γ)

data Typ
  = TNat
  | TArr Typ Typ
  | TVar Int
  | TFree Ident
  | TProd [Typ]
  | TSum [Typ]
  | TAll Typ
  deriving (Show, Eq)

data Exp
  = EZero
  | ESucc Exp
  | EFLam Typ Exp
  | EFApp Exp Exp
  | EVar Int
  | EFree Ident
  | ETupl [Exp]
  | EProj Exp Int
  | ECase Exp [Exp]
  | EInj  Int Exp
  | ETLam Exp
  | ETApp Exp Typ
  deriving (Show, Eq)

data ℕ = Z | S ℕ
  deriving (Eq, Show)

natToInt :: ℕ -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

data Val (τ :: Typ) where
  VNat :: γ ⊢ 'TNat -> Val 'TNat
  VLam :: STyp τ₁ -> (τ₁ : γ) ⊢ τ₂ -> Val (TArr τ₁ τ₂)
  -- | VProd [Exp]
  -- | VSum Int Exp
  -- | VLam Typ Exp
  -- | VTLam Exp
  -- deriving (Show, Eq)

instance Show (Val τ) where
  show (VNat _) = "VNat"
  show (VLam _ _) = "VLam"

