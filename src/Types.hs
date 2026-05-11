module Types
  ( module Types,
    Ident (..),
    module Data.Type.Equality
  )
where

import Data.Type.Equality

import Kx.Abs (Ident (..))

-- | START: Abstract Syntax

-- | The primitive types supported by KX
data Typ
  = TNat
  | TArr Typ Typ
  | TVar Int
  | TFree Ident
  | TProd [Typ]
  | TSum [Typ]
  | TAll Typ
  deriving (Show, Eq)

-- | Exp defines the abstract syntax of KX
data Exp
  = EZero
  | ESucc Exp
  | EFLam Ident Typ Exp
  | EFApp Exp Exp
  | -- TODO: Remove the Int, we don't need it anymore
    EVar Int Ident
  | EFree Ident
  | ETupl [Exp]
  | EProj Exp Int
  | ECase Exp [Exp]
  | EInj Int Exp
  | ETLam Exp
  | ETApp Exp Typ
  deriving (Show, Eq)

-- | START: Typed syntax and proof types

-- | Well typed Term definitions
data (γ :: [Typ]) ⊢ (τ :: Typ) where
  Var :: τ ∈ γ -> γ ⊢ τ
  Zero :: γ ⊢ 'TNat
  Succ :: γ ⊢ TNat -> γ ⊢ TNat
  Lam :: STyp τ₁ -> (τ₁ : γ) ⊢ τ₂ -> γ ⊢ 'TArr τ₁ τ₂
  App :: γ ⊢ 'TArr τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂
  -- TODO: Define the rest of the terms
  -- All :: ??
  -- Free :: ??

-- | Membership proofs
data (τ :: Typ) ∈ (γ :: [Typ]) where
  Here :: τ ∈ (τ : γ)
  There :: τ ∈ γ -> τ ∈ (τ' : γ)

-- | Absurdity - use when a case is impossible
absurdVar :: (τ ∈ '[]) -> a
absurdVar = \case {}

-- | Singleton type
data STyp (τ :: Typ) where
  SNat :: STyp 'TNat
  SArr :: STyp τ₁ -> STyp τ₂ -> STyp ('TArr τ₁ τ₂)
  -- TODO: Define the rest of the STyps

-- | Type for producing STyps without a polymorphic param.
-- Allows for runtime unpacking of types without compile-time checking
data SomeSTyp where
  SomeSTyp :: STyp τ -> SomeSTyp

-- | Wrap a Typ in SomeSTyp
toSTyp :: Typ -> SomeSTyp
toSTyp TNat = SomeSTyp SNat
toSTyp (TArr a r) = case (toSTyp a, toSTyp r) of
  (SomeSTyp sa, SomeSTyp sr) -> SomeSTyp $ SArr sa sr
toSTyp _ = undefined

instance Show (STyp τ) where
  show SNat = show TNat
  show (SArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

-- | Check equality between two STyps
typEq :: STyp τ₁ -> STyp τ₂ -> Maybe (τ₁ :~: τ₂)
typEq SNat SNat = Just Refl
typEq (SArr a b) (SArr c d) = do
  Refl <- typEq a c
  Refl <- typEq b d
  return Refl
typEq _ _ = Nothing

-- | START: Value types, produced by the evaluator

data Val (τ :: Typ) where
  VNat :: γ ⊢ 'TNat -> Val 'TNat
  VLam :: STyp τ₁ -> (τ₁ : '[]) ⊢ τ₂ -> Val (TArr τ₁ τ₂)

  -- TODO: Implement these values
  -- VProd [Exp]
  -- VSum Int Exp
  -- VLam Typ Exp
  -- VTLam Exp

instance Show (Val τ) where
  show (VNat _) = "VNat"
  show (VLam _ _) = "VLam"

data ℕ = Z | S ℕ
  deriving (Eq, Show)

natToInt :: ℕ -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n
