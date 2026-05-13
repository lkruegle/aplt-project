module Types
  ( module Types,
    Ident (..),
    module Data.Type.Equality,
  )
where

import qualified Data.Kind as K
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

data Kind where
  KTyp :: Kind
  KArr :: Kind -> Kind -> Kind

data Type (κ :: [Kind]) where
  Nat :: Type κ
  Arr :: Type κ -> Type κ -> Type κ
  -- VarType :: k ∈ κ -> Type κ
  VarType :: forall k κ. (k ∈ κ) -> Type κ
  Forall :: Kind -> Type (k : κ) -> Type κ

-- | Well typed Term definitions
data Term (κ :: [Kind]) (γ :: [Type κ]) (τ :: Type κ) where
  Zero :: Term κ γ Nat
  Succ :: Term κ γ Nat -> Term κ γ Nat
  Lam :: SType τ₁ -> Term κ (τ₁ : γ) τ₂ -> Term κ γ (Arr τ₁ τ₂)
  App :: Term κ γ (Arr τ₁ τ₂) -> Term κ γ τ₁ -> Term κ γ τ₂
  Var :: τ ∈ γ -> Term κ γ τ
  -- TypeVar :: (k ∈ κ) -> Term κ γ (VarType k)
  TypeVar :: forall k κ γ (i :: k ∈ κ). Term κ γ (VarType i)
  TypeLam :: Term (k : κ) γ τ -> Term κ γ (Forall k τ)
  -- TODO: Need type level weakening and subst
  -- TypeApp :: Term κ γ (Forall k) -> SType r -> Term κ γ τ

instance Show (Term κ γ τ) where
  show (Var x) = "(Var " <> show x <> ")"
  show Zero = "Zero"
  show (Succ e) = "(Succ " <> show e <> ")"
  show (Lam t body) = "λx : " <> show t <> " . " <> show body
  show (App f a) = "(" <> show f <> ")(" <> show a <> ")"

-- TODO: Define the rest of the terms
-- All :: ??
-- Free :: ??

-- | Membership proofs
data (τ :: a) ∈ (γ :: [a]) where
  Here :: τ ∈ (τ : γ)
  There :: τ ∈ γ -> τ ∈ (τ' : γ)

instance Show (τ ∈ γ) where
  show Here = "x"
  show x = "x" <> show (depth x)

depth :: forall τ γ. τ ∈ γ -> Int
depth Here = 0
depth (There x) = 1 + depth x

-- | Absurdity - use when a case is impossible
absurdVar :: (τ ∈ '[]) -> a
absurdVar = \case {}

-- | Singleton type
data SType :: forall κ. Type κ -> K.Type where
  SNat :: SType 'Nat
  SArr :: SType τ₁ -> SType τ₂ -> SType ('Arr τ₁ τ₂)

data SKind (k :: Kind) where
  SKTyp :: SKind 'KTyp
  SKArr :: SKind k₁ -> SKind k₂ -> SKind ('KArr k₁ k₂)

-- TODO: Define the rest of the STypes

-- | Type for producing STypes without a polymorphic param.
-- Allows for runtime unpacking of types without compile-time checking
data SomeSType (κ :: [Kind]) where
  SomeSType :: SType (τ :: Type κ) -> SomeSType κ

-- | Wrap a Typ in SomeSType
toSType :: Type κ -> SomeSType κ
toSType Nat = SomeSType SNat
toSType (Arr a r) = case (toSType a, toSType r) of
  (SomeSType sa, SomeSType sr) -> SomeSType $ SArr sa sr
toSType _ = undefined

instance Show (SType τ) where
  show SNat = show TNat
  show (SArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

-- | Check equality between two STypes
typEq :: SType τ₁ -> SType τ₂ -> Maybe (τ₁ :~: τ₂)
typEq SNat SNat = Just Refl
typEq (SArr a b) (SArr c d) = do
  Refl <- typEq a c
  Refl <- typEq b d
  return Refl
typEq _ _ = Nothing

-- | START: Value types, produced by the evaluator
data Val (τ :: Type κ) where
  VNat :: Term κ γ 'Nat -> Val 'Nat
  VLam :: SType τ₁ -> Term κ (τ₁ : '[]) τ₂ -> Val (Arr τ₁ τ₂)

-- TODO: Implement these values
-- VProd [Exp]
-- VSum Int Exp
-- VLam Typ Exp
-- VTLam Exp

instance Show (Val τ) where
  show (VNat e) = "ℕ " <> show e
  show (VLam a b) = show $ Lam a b
