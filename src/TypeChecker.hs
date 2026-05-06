module TypeChecker (typecheck) where

import Types
import BookKeeping hiding (bindTyp)

-- | Entrypoint for the typechecker
--
-- Given a desugared expression, infer the type the expression will evaluate
-- to.
typecheck :: Exp -> Either String Typ
typecheck = infer emptyContext

-- | The Context type for the type checker.
--
-- Binds type variables and terms to types.
type Ctx = Context Typ

-- | Bind a type variable in the context.
--
-- This function binds a new "current" type variable and shifts all existing
-- type variables up by 1 in response.
bindTyp :: Ctx -> Ctx
bindTyp c = c { boundTyps = TVar 0 : map (shiftTyp 0) (boundTyps c) }

-- | Looks up the type of the type variable at the given de bruijn index.
lookupTyp :: Int -> Ctx -> Either String Typ
lookupTyp i c = case lookupTyp' i c of
  Just t -> Right t
  Nothing -> Left "Out of scope type"

-- | Looks up the type of the term variable at the given de bruijn index.
lookupTerm :: Int -> Ctx -> Either String Typ
lookupTerm i c = case lookupTerm' i c of
  Just t -> Right t
  Nothing -> Left "Out of scope term"

guard :: String -> Bool -> Either String ()
guard s b = if b then pure () else Left s

check :: Ctx -> Typ -> Exp -> Either String ()
check c ty (EInj i e) = case ty of
    TSum taus | i < 0 -> Left $ "Injection index must be greater then zero"
              | i < length taus -> check c (taus !! i) e
              | otherwise -> Left $ "Injection index " <> show i <> " is out of bound for: " <> show ty
    _ -> Left "Inferred type does not meet expected type." 
check c ty (ECase es e) = do
  ty' <- infer c e
  case ty' of
    TSum tys | length tys == length es -> 
               let go [] [] = pure ()
                   go (e:es) (ty:tys) = do
                    check (bindTerm ty c) ty e
                    go es tys
                in go es tys
             | otherwise -> Left "Number of cases does not match sum type"
    _ -> Left "Case distinction on non sum-type."
check c ty exp = do
  ty' <- infer c exp
  guard "Inferred type does not meet expected type." (ty == ty')

-- | Infer the type of the given expression in the given context.
--
-- If the expression and context are well formed, this method will evaluate to
-- the type of the expression.
infer :: Ctx -> Exp -> Either String Typ
infer c (EVar i) = lookupTerm i c
infer _ (EFree (Ident x)) = Left $ "Free Variable " <> x
infer c (EFLam tau1 e) = do
  wellFormed c tau1
  tau2 <- infer (bindTerm tau1 c) e
  Right (TArr tau1 tau2)
infer c (EFApp f e) = do
  tf <- infer c f
  case tf of
    TArr t1 t2 -> do
      check c t1 e
      pure t2
    _ -> Left "Applied argument to non-lambda"
infer c (ETLam e) = do
  tau <- infer (bindTyp c) e
  Right $ TAll tau
infer c (ETApp e tau) = do
  wellFormed c tau
  tau' <- infer c e
  case tau' of
    TAll body -> Right $ substTyp tau body
    _ -> Left $ "Type application failed, " <> show tau <> " cannot be substituted in " <> show tau'
infer _ EZero = Right TNat
infer c (ESucc e) = do
  tau <- infer c e
  case tau of
    TNat -> Right TNat
    _ -> Left $ "Succ applied to non-nat: " <> show tau
infer c (ETupl es) = do
  taus <- mapM (infer c) es
  Right (TProd taus)
infer c (EProj e i) = do
  tau <- infer c e
  case tau of
    TProd taus | i < 0 -> Left $ "Projection index must be greater then zero"
               | i < length taus -> Right (taus !! i)
               | otherwise -> Left $ "Projection index " <> show i <> " is out of bound for: " <> show tau
    _ -> Left $ "Projection applied to non-product: " <> show tau
infer c e = Left $ "Cannot infer type of " <> show e

-- | Check that the given type is well-formed.
wellFormed :: Ctx -> Typ -> Either String ()
wellFormed c (TVar i) = lookupTyp i c >> Right ()
wellFormed c (TArr t1 t2) = wellFormed c t1 >> wellFormed c t2
wellFormed c (TAll t) = wellFormed (bindTyp c) t
wellFormed c (TProd taus) = mapM_ (wellFormed c) taus
wellFormed c (TSum taus) = mapM_ (wellFormed c) taus
wellFormed _ _ = Right ()
