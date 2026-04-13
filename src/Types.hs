module Types where

import Kx.Abs (Ident(..), Typ(..))

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
  Lam :: STyp τ₁ -> (τ₁:γ) ⊢ τ₂ -> γ ⊢ 'TFun τ₁ τ₂
  App :: γ ⊢ 'TFun τ₁ τ₂ -> γ ⊢ τ₁ -> γ ⊢ τ₂
  Let :: γ ⊢ τ₁ -> (τ₁:γ) ⊢ τ₂ -> γ ⊢ τ₂

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

data Ctx (γ :: [Typ]) where
  EmptyC :: Ctx '[]
  ConsC :: Ident -> STyp τ -> Ctx γ -> Ctx (τ:γ)

lookupCtx :: Ident -> Ctx γ -> Maybe (Found γ)
lookupCtx _ EmptyC = Nothing
lookupCtx n (ConsC n' typ ctx)
  | n == n' = Just $ Found typ Here
  | otherwise = case lookupCtx n ctx of
    Nothing -> Nothing
    (Just (Found typ' idx)) -> Just (Found typ' (There idx))

extractSTyp :: Ctx γ -> γ ⊢ τ -> STyp τ
extractSTyp (ConsC _ typ ctx) (Var idx) = case idx of
  Here -> typ
  There idx' -> extractSTyp ctx (Var idx')
extractSTyp _ Z = SNat
extractSTyp _ (S _) = SNat
extractSTyp ctx (RecN bt _ _) = extractSTyp ctx bt
extractSTyp ctx (Lam atyp rterm) =
  let rtyp = extractSTyp (ConsC (Ident "") atyp ctx) rterm
   in SFun atyp rtyp
extractSTyp ctx (App fterm _) = case extractSTyp ctx fterm of
  SFun atyp rtyp -> rtyp
  _ -> error "Impossible in well typed terms"
extractSTyp ctx (Let bterm iterm) =
  let btyp = extractSTyp ctx bterm
   in extractSTyp (ConsC (Ident "") btyp ctx) iterm

data Inferred (γ :: [Typ]) where
  Inferred :: γ ⊢ τ -> Inferred γ
