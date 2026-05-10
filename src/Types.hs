module Types
  ( module Types,
    Ident (..),
  )
where

import Kx.Abs (Ident (..))

data (γ :: [Typ]) ⊢ (τ :: Typ) where
  Var :: τ ∈ γ -> γ ⊢ τ
  Zero :: γ ⊢ 'TNat
  Succ :: γ ⊢ TNat -> γ ⊢ TNat
  Lam :: STyp τ₁ -> (τ₁ : γ) ⊢ τ₂ -> γ ⊢ 'TArr τ₁ τ₂
  App :: γ ⊢ 'TArr τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂

-- TODO: Define the rest of the terms
-- All :: ??
-- Free :: ??

data (τ :: Typ) ∈ (γ :: [Typ]) where
  Here :: τ ∈ (τ : γ)
  There :: τ ∈ γ -> τ ∈ (τ' : γ)

data Found (γ :: [Typ]) where
  Found :: STyp τ -> τ ∈ γ -> Found γ

type Env (γ :: [Typ]) = forall (τ :: Typ). τ ∈ γ -> Val τ

emptyEnv :: Env '[]
emptyEnv = \case {}

consEnv :: Env γ -> Val τ -> Env (τ : γ)
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
  ConsC :: Ident -> STyp τ -> Ctx γ -> Ctx (τ : γ)

lookupCtx :: Ident -> Ctx γ -> Maybe (Found γ)
lookupCtx _ EmptyC = Nothing
lookupCtx n (ConsC n' typ ctx)
  | n == n' = Just $ Found typ Here
  | otherwise = case lookupCtx n ctx of
      Nothing -> Nothing
      (Just (Found typ' idx)) -> Just (Found typ' (There idx))

absurdVar :: (τ ∈ '[]) -> a
absurdVar = \case {}

extractSTyp :: Ctx γ -> γ ⊢ τ -> STyp τ
extractSTyp (ConsC _ typ ctx) (Var idx) = case idx of
  Here -> typ
  There idx' -> extractSTyp ctx (Var idx')
extractSTyp EmptyC (Var x) = absurdVar x
extractSTyp _ Zero = SNat
extractSTyp _ (Succ _) = SNat
extractSTyp ctx (Lam atyp rterm) =
  let rtyp = extractSTyp (ConsC (Ident "") atyp ctx) rterm
   in SArr atyp rtyp
extractSTyp ctx (App fterm _) = case extractSTyp ctx fterm of
  SArr _ rtyp -> rtyp
  -- TODO: This should be able to use something like absurd
  _ -> error "Impossible in well typed terms"

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

data ℕ = Z | S ℕ
  deriving (Eq, Show)

natToInt :: ℕ -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

data Val (τ :: Typ) where
  VNat :: γ ⊢ 'TNat -> Val 'TNat
  VLam :: STyp τ₁ -> (τ₁ : '[]) ⊢ τ₂ -> Val (TArr τ₁ τ₂)

-- \| VProd [Exp]
-- \| VSum Int Exp
-- \| VLam Typ Exp
-- \| VTLam Exp
-- deriving (Show, Eq)

instance Show (Val τ) where
  show (VNat _) = "VNat"
  show (VLam _ _) = "VLam"
