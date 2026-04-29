module TypeChecker (typecheck) where

import Types
import BookKeeping

typecheck :: Exp -> Either String Typ
typecheck = infer emptyContext

type Ctx = Context Typ

bindTyp :: Ctx -> Ctx
-- TODO: Implement shifting to bind a fresh Typ with i=0 and shift all others up
bindTyp c = c { boundTyps = TVar 0 : map (shiftTyp 0) (boundTyps c) }

bindTerm :: Typ -> Ctx -> Ctx
bindTerm = bindTerm'

lookupTyp :: Int -> Ctx -> Either String Typ
lookupTyp i c = case lookupTyp' i c of
  Just t -> Right t
  Nothing -> Left "Out of scope type"

lookupTerm :: Int -> Ctx -> Either String Typ
lookupTerm i c = case lookupTerm' i c of
  Just t -> Right t
  Nothing -> Left "Out of scope term"

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
  tau <- infer c e
  Right $ TAll tau
infer c (ETApp e tau) = do
  check c tau
  tau' <- infer c e
  case tau' of
    TAll body -> Right $ substTyp tau body
    _ -> Left "Type application failed"

check :: Ctx -> Typ -> Either String ()
check c (TVar i) = lookupTyp i c >> Right ()
check c (TArr t1 t2) = check c t1 >> check c t2
check c (TAll t) = check (bindTyp c) t
check _ _ = Right ()
