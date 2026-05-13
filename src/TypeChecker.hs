module TypeChecker (typecheck, Inferred (..)) where

import Types
import Prelude hiding (exp)

-- | Entrypoint for the typechecker
--
-- Given a desugared expression, infer the type the expression will evaluate
-- to.
typecheck :: Exp -> M (Inferred '[])
typecheck = infer EmptyC

-- | Construct a proof that the given expression is a well typed term in the
-- given context if such a term can be constructed.
infer :: Ctx γ -> Exp -> M (Inferred γ)
infer _ EZero = Right $ Inferred Zero
infer c (ESucc e) = do
  term <- check c SNat e
  Right $ Inferred (Succ term)
infer c (EVar _ i@(Ident n)) = case lookupCtx i c of
  Nothing -> Left $ "No variable bound with name " ++ n
  Just (Found _ x) -> Right $ Inferred (Var x)
infer c (EFLam x t body) = case toSType t of
  SomeSType atyp -> do
    Inferred fterm <- infer (ConsC x atyp c) body
    Right $ Inferred (Lam atyp fterm)
infer c (EFApp func arg) = do
  Inferred fterm <- infer c func
  case extractSType c fterm of
    SArr atyp _ -> do
      case check c atyp arg of
        Right aterm -> do
          Right $ Inferred (App fterm aterm)
        Left err -> Left $ "Function applied to incorrect argument type. " ++ err
    _ -> Left "Function application applied to non-function term"
infer _ _ = undefined

-- | Check that the given expression has some expected type in the given context.
check :: Ctx κ γ -> SType τ -> Exp -> M (Term κ γ τ)
check c typ exp = case infer c exp of
  Left err -> Left err
  Right (Inferred term) ->
    let typ' = extractSType c term
     in case typEq typ typ' of
          Nothing -> Left $ unwords ["Expected:", show typ, "Got:", show typ']
          Just Refl -> Right term

-- START: Types and helper functions for typechecking

-- | TypeChecker Monad
type M a = Either String a

-- | Inferred type wrapper
-- Required to wrap τ so that it can be unpacked at runtime.
data Inferred (κ :: [Kind]) (γ :: [Type κ]) where
  Inferred :: Term κ γ τ -> Inferred κ γ

-- | The Context type for the typechecker.
data Ctx (κ :: [Kind]) (γ :: [Type κ]) where
  EmptyC :: Ctx '[] '[]
  ConsC :: Ident -> SType τ -> Ctx κ γ -> Ctx κ (τ : γ)
  ConsK :: Ident -> SKind k -> Ctx κ γ -> Ctx (k : κ) γ

-- | Runtime wrapper for results of looking up a variable in the context.
data Found (κ :: [Kind]) (γ :: [Type κ]) where
  Found :: SType τ -> τ ∈ γ -> Found κ γ

-- | Lookup the type of the given identifier in the context.
lookupCtx :: Ident -> Ctx κ γ -> Maybe (Found κ γ)
lookupCtx _ EmptyC = Nothing
lookupCtx n (ConsC n' typ ctx)
  | n == n' = Just $ Found typ Here
  | otherwise = case lookupCtx n ctx of
      Nothing -> Nothing
      (Just (Found typ' idx)) -> Just (Found typ' (There idx))

-- | Given a context and a term in that context, produce the Singleton type
-- for the term.
extractSType :: Ctx κ γ -> Term κ γ τ -> SType τ
extractSType (ConsC _ typ ctx) (Var idx) = case idx of
  Here -> typ
  There idx' -> extractSType ctx (Var idx')
extractSType EmptyC (Var x) = absurdVar x
extractSType _ Zero = SNat
extractSType _ (Succ _) = SNat
extractSType ctx (Lam atyp rterm) =
  let rtyp = extractSType (ConsC (Ident "") atyp ctx) rterm
   in SArr atyp rtyp
extractSType ctx (App fterm _) = case extractSType ctx fterm of
  SArr _ rtyp -> rtyp
