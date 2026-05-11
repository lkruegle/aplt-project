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

-- | Check that the given expression has some expected type in the given context.
check :: Ctx γ -> STyp τ -> Exp -> M (γ ⊢ τ)
check c typ exp = case infer c exp of
  Left err -> Left err
  Right (Inferred term) ->
    let typ' = extractSTyp c term
     in case typEq typ typ' of
          Nothing -> Left $ unwords ["Expected:", show typ, "Got:", show typ']
          Just Refl -> Right term

-- START: Types and helper functions for typechecking

-- | TypeChecker Monad
type M a = Either String a

-- | Inferred type wrapper
-- Required to wrap τ so that it can be unpacked at runtime.
data Inferred (γ :: [Typ]) where
  Inferred :: γ ⊢ τ -> Inferred γ

-- | The Context type for the typechecker.
data Ctx (γ :: [Typ]) where
  EmptyC :: Ctx '[]
  ConsC :: Ident -> STyp τ -> Ctx γ -> Ctx (τ : γ)

-- | Runtime wrapper for results of looking up a variable in the context.
data Found (γ :: [Typ]) where
  Found :: STyp τ -> τ ∈ γ -> Found γ

-- | Lookup the type of the given identifier in the context.
lookupCtx :: Ident -> Ctx γ -> Maybe (Found γ)
lookupCtx _ EmptyC = Nothing
lookupCtx n (ConsC n' typ ctx)
  | n == n' = Just $ Found typ Here
  | otherwise = case lookupCtx n ctx of
      Nothing -> Nothing
      (Just (Found typ' idx)) -> Just (Found typ' (There idx))

-- | Given a context and a term in that context, produce the Singleton type
-- for the term.
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
