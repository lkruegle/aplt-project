module Types where

import Kx.Abs (Ident, Typ(..))

data ℕ = Zero | Succ ℕ
  deriving (Eq, Show)

natToInt :: ℕ -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + (natToInt n)

data Val (τ :: Typ) where
  VNat :: ℕ -> Val TNat
  VFun :: (Val τ₁ -> Val τ₂) -> Val ('TFun τ₁ τ₂)

instance Show (Val τ) where
  show (VNat n) = show $ natToInt n
  show (VFun _) = "λ"

data (τ :: Typ) ∈ (γ :: [Typ]) where
  Here :: τ ∈ (τ:γ)
  There :: τ ∈ γ -> τ ∈ (τ':γ)

data (γ :: [Typ]) ⊢ (τ :: Typ) where
  Var :: τ ∈ γ -> γ ⊢ τ
  Z :: γ ⊢ 'TNat
  S :: γ ⊢ 'TNat -> γ ⊢ 'TNat
  RecN :: γ ⊢ τ -> ('TNat:τ:γ) ⊢ τ -> γ ⊢ 'TNat -> γ ⊢ τ
  Lam :: (τ₁:γ) ⊢ τ₂ -> γ ⊢ 'TFun τ₁ τ₂
  App :: γ ⊢ 'TFun τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂
  Let :: γ ⊢ τ₁ -> (τ₁:γ) ⊢ τ₂ -> γ ⊢ τ₂

type Env (γ :: [Typ]) = forall (τ :: Typ). τ ∈ γ -> Val τ

emptyEnv :: Env '[]
emptyEnv = \case

consEnv :: Env γ -> Val τ -> Env (τ:γ)
consEnv e x Here = x
consEnv e x (There p) = e p

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

data STyp (τ :: Typ) where
  SNat :: STyp 'TNat
  SFun :: STyp τ₁ -> STyp τ₂ -> STyp ('TFun τ₁ τ₂)

data SomeSTyp where
  SomeSTyp :: STyp τ -> SomeSTyp

toSTyp :: Typ -> SomeSTyp
toSTyp (TFun a r) = case (toSTyp a, toSTyp r) of
  (SomeSTyp sa, SomeSTyp sr) -> SomeSTyp $ SFun sa sr
toSTyp TNat = SomeSTyp SNat

instance Show (STyp τ) where
  show SNat = show TNat
  show (SFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
