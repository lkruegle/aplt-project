module Types
  ( module Types,
    Ident (..),
    module Data.Type.Equality,
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

-- | List of terms of a given list of types in a context
--  
--  used for generalised products
data TypList m (ts :: [Typ]) where
  Emp :: TypList m '[]
  Cons :: m τ -> TypList m tys -> TypList m (τ:tys)

instance Show (TypList m ts) where
  show trs = "TypList" -- TODO: is it possible to get a proper show instance here?

-- | Well typed Term definitions
data (γ :: [Typ]) ⊢ (τ :: Typ) where
  Zero :: γ ⊢ 'TNat
  Succ :: γ ⊢ TNat -> γ ⊢ TNat
  Lam :: STyp τ₁ -> (τ₁ : γ) ⊢ τ₂ -> γ ⊢ 'TArr τ₁ τ₂
  App :: γ ⊢ 'TArr τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂
  Var :: τ ∈ γ -> γ ⊢ τ
  Prod :: TypList ((⊢) γ) ts -> γ ⊢ TProd ts
  Proj :: γ ⊢ TProd ts -> τ ∈ ts -> γ ⊢ τ

-- TODO: Define the rest of the terms
-- All :: ??
-- Free :: ??

instance Show (γ ⊢ τ) where
  show (Var x) = "(Var " <> show x <> ")"
  show Zero = "Zero"
  show (Succ e) = "(Succ " <> show e <> ")"
  show (Lam t body) = "λx : " <> show t <> " . " <> show body
  show (App f a) = "(" <> show f <> ")(" <> show a <> ")"
  show (Prod tl) = "<" <> show tl <> ">"
  show (Proj tr x) = show tr <> "." <> show (depth x)

-- | Membership proofs
data (τ :: Typ) ∈ (γ :: [Typ]) where
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
data STyp (τ :: Typ) where
  SNat  :: STyp 'TNat
  SArr  :: STyp τ₁ -> STyp τ₂ -> STyp ('TArr τ₁ τ₂)
  SProd :: TypList STyp ts -> STyp ('TProd ts)

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
toSTyp (TProd tys) = SomeSTyp $ SProd (go tys)
  where
    go :: [Typ] -> TypList STyp ts
    go [] = Emp
    go (ty:tys) = Cons (let SomeSTyp st = toSTyp ty in st) (go tys)
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
  show (VNat e) = "ℕ " <> show e
  show (VLam a b) = show $ Lam a b
