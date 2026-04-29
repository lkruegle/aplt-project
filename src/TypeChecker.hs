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

-- | Infer the type of the given expression in the given context.
--
-- If the expression and context are well formed, this method will evaluate to
-- the type of the expression.
infer :: Ctx -> Exp -> Either String Typ
infer c (EVar i) = lookupTerm i c
infer _ (EFree (Ident x)) = Left $ "Free Variable " <> x
infer c (EFLam tau1 e) = do
  check c tau1
  tau2 <- infer (bindTerm tau1 c) e
  Right (TArr tau1 tau2)
infer c (EFApp f e) = do
  tf <- infer c f
  te <- infer c e
  case tf of
    TArr t1 t2 | t1 == te -> Right t2
    TArr _ _ -> Left "Argument of incorrect type"
    _ -> Left "Applied argument to non-lambda"
infer c (ETLam e) = do
  tau <- infer (bindTyp c) e
  Right $ TAll tau
infer c (ETApp e tau) = do
  check c tau
  tau' <- infer c e
  case tau' of
    TAll body -> Right $ substTyp tau body
    _ -> Left "Type application failed"

-- | Check that the given type is well-formed.
check :: Ctx -> Typ -> Either String ()
check c (TVar i) = lookupTyp i c >> Right ()
check c (TArr t1 t2) = check c t1 >> check c t2
check c (TAll t) = check (bindTyp c) t
check _ _ = Right ()
