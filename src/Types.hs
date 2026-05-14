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

data Tuple (γ :: [Typ]) (τs :: [Typ]) where
  TNil :: Tuple γ '[]
  TCons :: Term γ τ -> Tuple γ τs -> Tuple γ (τ : τs)

instance Show (Tuple γ τs) where
  show tup = "<" <> go tup <> ">"
    where
      go :: Tuple γ' τs' -> String
      go TNil = ""
      go (TCons t ts) = show t <> case ts of
        TNil -> ""
        _ -> ", " <> go ts

data Cases (γ :: [Typ]) (τs :: [Typ]) (τ :: Typ) where
  CNil :: Cases γ τs τ
  CCase :: Term (τ' : γ) τ -> Cases γ τs τ -> Cases γ (τ' : τs) τ

instance Show (Cases γ τs τ) where
  show cases = "{" <> go 0 cases <> "}"
    where
      go :: Int -> Cases γ' τs' τ' -> String
      go _ CNil = ""
      go i (CCase c cs) =
        show i <> " ☞  " <> show c
        <> case cs of
          CNil -> ""
          _    -> ", " <> go (i + 1) cs

-- | Well typed Term definitions
data Term (γ :: [Typ]) (τ :: Typ) where
  Zero :: Term γ 'TNat
  Succ :: Term γ 'TNat -> Term γ 'TNat
  Lam :: STyp τ₁ -> Term (τ₁ : γ) τ₂ -> Term γ ('TArr τ₁ τ₂)
  App :: Term γ ('TArr τ₁ τ₂) -> Term γ τ₁ -> Term γ τ₂
  Var :: τ ∈ γ -> Term γ τ
  Tup :: Tuple γ τs -> Term γ ('TProd τs)
  Proj :: τ ∈ τs -> Term γ ('TProd τs) -> Term γ τ
  Inj :: τ ∈ τs -> Term γ τ -> STuple τs -> Term γ ('TSum τs)
  Case :: Term γ ('TSum τs) -> Cases γ τs τ -> Term γ τ

instance Show (Term γ τ) where
  show (Var x) = "(Var " <> show x <> ")"
  show Zero = "Zero"
  show (Succ e) = "(Succ " <> show e <> ")"
  show (Lam t body) = "λx : " <> show t <> " . " <> show body
  show (App f a) = "(" <> show f <> ")(" <> show a <> ")"
  show (Tup tup) = show tup
  show (Proj idx tup) = show tup <> "." <> show idx
  show (Inj idx t _) = show idx <> "." <> show t
  show (Case t c) = unwords ["case", show t, show c]

-- | Membership proofs
data (τ :: a) ∈ (γ :: [a]) where
  Here :: τ ∈ (τ : γ)
  There :: τ ∈ γ -> τ ∈ (τ' : γ)

instance Show (τ ∈ γ) where
  show = show . depth

depth :: forall τ γ. τ ∈ γ -> Int
depth Here = 0
depth (There x) = 1 + depth x

-- | Absurdity - use when a case is impossible
absurdVar :: (τ ∈ '[]) -> a
absurdVar = \case {}

-- | Singleton type
data STyp (τ :: Typ) where
  SNat :: STyp 'TNat
  SArr :: STyp τ₁ -> STyp τ₂ -> STyp ('TArr τ₁ τ₂)
  SProd :: STuple τs -> STyp ('TProd τs)
  SSum :: STuple τs -> STyp ('TSum τs)

instance Show (STyp τ) where
  show SNat = "ℕ"
  show (SArr a b) = "(" ++ show a ++ " → " ++ show b ++ ")"
  show (SProd ts) = "<" <> show ts <> ">"
  show (SSum ts) = "[" <> show ts <> "]"

data STuple (τs :: [Typ]) where
  SNil :: STuple '[]
  SCons :: STyp τ -> STuple τs -> STuple (τ : τs)

instance Show (STuple τs) where
  show SNil = ""
  show (SCons t ts) = show t <> case ts of
    SNil -> ""
    _ -> ", " <> show ts

-- | Type for producing STyps without a polymorphic param.
-- Allows for runtime unpacking of types without compile-time checking
data SomeSTyp where
  SomeSTyp :: STyp τ -> SomeSTyp

data SomeSTuple where
  SomeSTuple :: STuple τs -> SomeSTuple

-- | Wrap a Typ in SomeSTyp
toSTyp :: Typ -> SomeSTyp
toSTyp TNat = SomeSTyp SNat
toSTyp (TArr a r) = case (toSTyp a, toSTyp r) of
  (SomeSTyp sa, SomeSTyp sr) -> SomeSTyp $ SArr sa sr
toSTyp (TProd []) = SomeSTyp $ SProd SNil
toSTyp (TProd tup) = case toSTuple tup of
  SomeSTuple stup -> SomeSTyp $ SProd stup
toSTyp _ = undefined

toSTuple :: [Typ] -> SomeSTuple
toSTuple [] = SomeSTuple SNil
toSTuple (t:ts) = case (toSTyp t, toSTuple ts) of
  (SomeSTyp t', SomeSTuple ts') -> SomeSTuple $ SCons t' ts'

-- | Check equality between two STyps
typEq :: STyp τ₁ -> STyp τ₂ -> Maybe (τ₁ :~: τ₂)
typEq SNat SNat = Just Refl
typEq (SArr a b) (SArr c d) = do
  Refl <- typEq a c
  Refl <- typEq b d
  return Refl
typEq (SProd a) (SProd b) = do
  Refl <- go a b
  return Refl
  where
    go :: STuple τs -> STuple τs' -> Maybe (τs :~: τs')
    go (SCons x xs) (SCons y ys) = do
      Refl <- typEq x y
      Refl <- go xs ys
      return Refl
    go SNil SNil = return Refl
    go _ _ = Nothing
typEq _ _ = Nothing

-- | START: Value types, produced by the evaluator
data Val (τ :: Typ) where
  Val :: Term '[] τ -> Val τ

instance Show (Val τ) where
  show (Val t) = show t
