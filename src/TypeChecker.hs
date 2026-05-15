module TypeChecker (typecheck, Inferred (..)) where

import Types
import Prelude hiding (exp)

-- | Entrypoint for the typechecker
--
-- Given a desugared expression, infer the type the expression will evaluate
-- to.
typecheck :: Exp -> M (Inferred '[])
typecheck = infer NilCtx


-- | Construct a proof that the given expression is a well typed term in the
-- given context if such a term can be constructed.
infer :: Ctx γ -> Exp -> M (Inferred γ)
infer c (ETAnn e t) = case toSTyp t of
  Some t' -> check c t' e >>= Right . Inferred
infer _ EZero = Right $ Inferred Zero
infer c (ESucc e) = do
  term <- check c SNat e
  Right $ Inferred (Succ term)
infer c (EVar _ i@(Ident n)) = case lookupCtx i c of
  Nothing -> Left $ "No variable bound with name " ++ n
  Just (Found _ x) -> Right $ Inferred (Var x)
infer c (EFLam x t body) = case toSTyp t of
  Some atyp -> do
    Inferred fterm <- infer (ConsCtx x atyp c) body
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
infer c (ETupl exps) = do
  InferredTuple tup <- inferTuple c exps
  Right $ Inferred $ Tup tup
infer c (EProj exp idx) = do
  Inferred term <- infer c exp
  case extractSTyp c term of
    (SProd stup) -> case lookupSTuple idx stup of
      Nothing -> error "Projection index out of bounds!"
      Just (Found _ mp) -> Right $ Inferred (Proj mp term)
    _ -> error "Projection applied to non-product type"
infer _ e = error $ "Cannot infer type of expression: " <> show e

data InferredTuple γ where
  InferredTuple :: Tuple γ τs -> InferredTuple γ

inferTuple :: Ctx γ -> [Exp] -> M (InferredTuple γ)
inferTuple _ [] = Right $ InferredTuple TNil
inferTuple c (e : es) = do
  Inferred t <- infer c e
  InferredTuple ts <- inferTuple c es
  return $ InferredTuple (TCons t ts)

-- | Check that the given expression has some expected type in the given context.
check :: Ctx γ -> STyp τ -> Exp -> M (Term γ τ)
check c (SArr atyp rtyp) (EFLam x _ body) = do
  term <- check (ConsCtx x atyp c) rtyp body
  Right $ Lam atyp term
check c st inj@(EInj i e) = case st of
  SSum ts -> case lookupSTuple i ts of
    Just (Found et idx) -> do
      Inferred term <- infer c e
      let at = extractSTyp c term
      case typEq at et of
        Just Refl -> Right $ Inj idx term ts
        Nothing ->
          Left $
            unwords
              [ "Sum Type mismatch:",
                show at,
                "/=",
                show et,
                "at index",
                show i,
                "in sum type",
                show st
              ]
    Nothing -> error "Index out of bounds for expected Sum type"
  _ -> Left $ "Expression " <> show inj <> " is not of expected type: " <> show st
check c t (ECase e cs) = do
  (Inferred sumt) <- infer c e
  let st = extractSTyp c sumt
  case st of
    SSum ts -> do
      cases <- checkCases c t ts cs
      pure $ Case sumt cases
    _ -> error $ "Expected sum type for cases, got :" <> show st
check c typ exp = case infer c exp of
  Left err -> Left err
  Right (Inferred term) ->
    let typ' = extractSTyp c term
     in case typEq typ typ' of
          Nothing -> Left $ unwords ["Expected:", show typ, "Got:", show typ']
          Just Refl -> Right term

checkCases :: Ctx γ -> STyp τ -> STuple τs -> [(Ident, Exp)] -> M (Cases γ τs τ)
checkCases _ _ SNil [] = Right CNil
checkCases c t (SCons t' ts') ((i, e):es) = do
  term <- check (ConsCtx i t' c) t e
  rest <- checkCases c t ts' es
  Right $ CCons term rest
checkCases _ _ _ _  = Left "Incorrect number of cases."

-- START: Types and helper functions for typechecking

-- | TypeChecker Monad
type M a = Either String a

-- | Inferred type wrapper
-- Required to wrap τ so that it can be unpacked at runtime.
data Inferred (γ :: [Typ]) where
  Inferred :: Term γ τ -> Inferred γ

-- | The Context type for the typechecker.
data Ctx (γ :: [Typ]) where
  NilCtx :: Ctx '[]
  ConsCtx :: Ident -> STyp τ -> Ctx γ -> Ctx (τ : γ)

-- | Runtime wrapper for results of looking up a variable in the context.
data Found (γ :: [Typ]) where
  Found :: STyp τ -> τ ∈ γ -> Found γ

-- | Lookup the type of the given identifier in the context.
lookupCtx :: Ident -> Ctx γ -> Maybe (Found γ)
lookupCtx _ NilCtx = Nothing
lookupCtx n (ConsCtx n' typ ctx)
  | n == n' = Just $ Found typ Here
  | otherwise = case lookupCtx n ctx of
      Nothing -> Nothing
      (Just (Found typ' idx)) -> Just (Found typ' (There idx))

lookupSTuple :: forall τs. Int -> STuple τs -> Maybe (Found τs)
lookupSTuple = go 0
  where
    go :: forall τs'. Int -> Int -> STuple τs' -> Maybe (Found τs')
    go _ _ SNil = Nothing
    go d i (SCons t ts)
      | d == i = Just $ Found t Here
      | otherwise = case go (d + 1) i ts of
          Nothing -> Nothing
          (Just (Found typ' idx)) -> Just (Found typ' (There idx))

-- | Given a context and a term in that context, produce the Singleton type
-- for the term.
extractSTyp :: forall γ τ. Ctx γ -> Term γ τ -> STyp τ
extractSTyp (ConsCtx _ typ ctx) (Var idx) = case idx of
  Here -> typ
  There idx' -> extractSTyp ctx (Var idx')
extractSTyp NilCtx (Var x) = absurdVar x
extractSTyp _ Zero = SNat
extractSTyp _ (Succ _) = SNat
extractSTyp ctx (Lam atyp rterm) =
  let rtyp = extractSTyp (ConsCtx (Ident "") atyp ctx) rterm
   in SArr atyp rtyp
extractSTyp ctx (App fterm _) = case extractSTyp ctx fterm of
  SArr _ rtyp -> rtyp
extractSTyp ctx (Tup tup) = SProd $ go tup
  where
    go :: Tuple γ τs -> STuple τs
    go TNil = SNil
    go (TCons t ts) = SCons (extractSTyp ctx t) (go ts)
extractSTyp ctx (Proj idx prod) =
  let SProd stup = extractSTyp ctx prod
   in go idx stup
  where
    go :: τ ∈ τs -> STuple τs -> STyp τ
    go Here (SCons st _) = st
    go (There i) (SCons _ sts) = go i sts
extractSTyp _ (Inj _ _ ts) = SSum ts
extractSTyp ctx (Case t cases) = case (extractSTyp ctx t, cases) of
  (SSum (SCons t' _), CCons c _) -> extractSTyp (ConsCtx (Ident "") t' ctx) c
  (SSum SNil, _) -> error "Cannot case on an empty Sum type"
  (_, CNil) -> error "Cannot proceed on empty cases"
