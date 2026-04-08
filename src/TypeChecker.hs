module TypeChecker (typecheck) where

import Data.Type.Equality
import Kx.Abs
import Types

type M a = Either String a

typecheck :: Exp -> M (Inferred '[])
typecheck exp = infer EmptyC exp

infer :: Ctx γ -> Exp -> M (Inferred γ)
infer ctx (ENatZ) = Right $ Inferred SNat Z
infer ctx (ENatS exp) = do
  term <- check ctx SNat exp
  Right $ Inferred SNat (S term)
infer ctx (ENat i) = Right $ Inferred SNat (natTerm i)
  where
    natTerm 0 = Z
    natTerm x = S $ natTerm (x - 1)
infer ctx (EVar id@(Ident n)) = case lookupCtx id ctx of
  Nothing -> Left $ "No variable bound with name " ++ n
  Just (Found typ idx) -> Right $ Inferred typ (Var idx)
infer ctx (ERec nat base x y rec) = do
  nTerm <- check ctx SNat nat
  Inferred typ bTerm <- infer ctx base
  rTerm <- check (ConsC x SNat (ConsC y typ ctx)) typ rec
  Right $ Inferred typ (RecN bTerm rTerm nTerm)
infer ctx (ELam id typA exp) = case toSTyp typA of
  SomeSTyp atyp -> do
    Inferred rtyp fterm <- infer (ConsC id atyp ctx) exp
    Right $ Inferred (SFun atyp rtyp) (Lam fterm)
infer ctx (EApl f a) = do
  Inferred ftyp fterm <- infer ctx f
  case ftyp of
    SFun atyp rtyp -> do
      case check ctx atyp a of
        Right aterm -> do
          Right $ Inferred rtyp (App fterm aterm)
        Left err -> Left $ "Function applied to incorrect argument type. " ++ err
    _ -> Left "Function application applied to non-function term"
infer ctx (ELet id vexp iexp) = do
  Inferred typ vterm <- infer ctx vexp
  Inferred typ' iterm <- infer (ConsC id typ ctx) iexp
  Right $ Inferred typ' $ Let vterm iterm

check :: Ctx γ -> STyp τ -> Exp -> M (γ ⊢ τ)
check ctx typ exp = case infer ctx exp of
  Left err -> Left err
  Right (Inferred typ' term) -> case typEq typ typ' of
    Nothing -> Left $ unwords [ "Expected:", show typ, "Got:", show typ' ]
    Just Refl -> Right term

typEq :: STyp τ1 -> STyp τ2 -> Maybe (τ1 :~: τ2)
typEq SNat      SNat        = Just Refl
typEq (SFun a b) (SFun c d) = do
  Refl <- typEq a c
  Refl <- typEq b d
  return Refl
typEq _ _                   = Nothing
