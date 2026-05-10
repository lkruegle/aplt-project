module TypeChecker (typecheck, Inferred (..)) where

import Data.Type.Equality
import Types
import Prelude hiding (exp)

data Inferred (γ :: [Typ]) where
  Inferred :: γ ⊢ τ -> Inferred γ

-- | Entrypoint for the typechecker
--
-- Given a desugared expression, infer the type the expression will evaluate
-- to.
typecheck :: Exp -> M (Inferred '[])
typecheck = infer EmptyC

type M a = Either String a

infer :: Ctx γ -> Exp -> M (Inferred γ)
infer c EZero = Right $ Inferred Zero
infer c (ESucc e) = do
  term <- check c SNat e
  Right $ Inferred (Succ term)
infer c (EVar _ i@(Ident n)) = case lookupCtx i c of
  Nothing -> Left $ "No variable bound with name " ++ n
  Just (Found _ x) -> Right $ Inferred (Var x)
infer c (EFLam x t body) = case toSTyp t of
  SomeSTyp atyp -> do
    Inferred fterm <- infer (ConsC x atyp c) body
    Right $ Inferred (Lam atyp fterm)
infer c (EFApp func arg) = do
  Inferred fterm <- infer c func
  case extractSTyp c fterm of
    SArr atyp _ -> do
      case check c atyp arg of
        Right aterm -> do
          Right $ Inferred (App fterm aterm)
        Left err -> Left $ "Function applied to incorrect argument type. " ++ err
    _ -> Left "Function application applied to non-function term"
infer _ _ = undefined

check :: Ctx γ -> STyp τ -> Exp -> M (γ ⊢ τ)
check c typ exp = case infer c exp of
  Left err -> Left err
  Right (Inferred term) ->
    let typ' = extractSTyp c term
     in case typEq typ typ' of
          Nothing -> Left $ unwords ["Expected:", show typ, "Got:", show typ']
          Just Refl -> Right term

typEq :: STyp τ₁ -> STyp τ₂ -> Maybe (τ₁ :~: τ₂)
typEq SNat SNat = Just Refl
typEq (SArr a b) (SArr c d) = do
  Refl <- typEq a c
  Refl <- typEq b d
  return Refl
typEq _ _ = Nothing
