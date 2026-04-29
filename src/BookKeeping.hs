module BookKeeping where

import Types

data Context a = Context { boundTyps :: [a], boundTerms :: [a] }

emptyContext :: Context a
emptyContext = Context [] []

bindTerm' :: a -> Context a -> Context a
bindTerm' x c = c { boundTerms = x : boundTerms c }

bindTyp' :: a -> Context a -> Context a
bindTyp' t c = c { boundTyps = t : boundTyps c }

lookupTerm' :: Int -> Context a -> Maybe a
lookupTerm' = lookupC boundTerms

lookupTyp' :: Int -> Context a -> Maybe a
lookupTyp' = lookupC boundTyps

lookupC :: (Context a -> [a]) -> Int -> Context a -> Maybe a
lookupC f i c = case drop i (f c) of
  (t:_) -> Just t
  [] -> Nothing

shiftTyp :: Int -> Typ -> Typ
shiftTyp c (TVar i)
  | i >= c = TVar $ i + 1
  | otherwise = TVar i
shiftTyp c (TArr arg ret) = TArr (shiftTyp c arg) (shiftTyp c ret)
shiftTyp c (TAll t) = TAll $ shiftTyp (c + 1) t
shiftTyp _ t = t

substTyp :: Typ -> Typ -> Typ
substTyp x (TVar 0) = x
substTyp x (TVar i) = TVar (i - 1) -- binding shifts vars down
substTyp x (TArr arg ret) = TArr (substTyp x arg) (substTyp x ret)
substTyp x (TAll t) = TAll (substTyp x t)
substTyp _ t = t

shiftExp :: Int -> Exp -> Exp
shiftExp c (EVar i)
  | i >= c = EVar $ i + 1
  | otherwise = EVar i
shiftExp c (EFLam t e) = EFLam t $ shiftExp (c + 1) e
shiftExp c (EFApp f a) = EFApp (shiftExp c f) (shiftExp c a)
shiftExp c (ETLam e) = ETLam $ shiftExp c e
shiftExp c (ETApp e t) = ETApp (shiftExp c e) t
shiftExp _ e = e

substExp :: Exp -> Exp -> Exp
substExp e (EVar 0) = e
substExp e (EVar i) = EVar $ i - 1
substExp e (EFLam t body) = EFLam t $ substExp e body
substExp e (EFApp fun arg) = EFApp (substExp e fun) (substExp e arg)
substExp e (ETLam body) = ETLam $ substExp e body
substExp e (ETApp body t) = ETApp (substExp e body) t
substExp _ e' = e'

substTypInExp :: Typ -> Exp -> Exp
substTypInExp t (ETLam body) = ETLam $ substTypInExp (shiftTyp 0 t) body
substTypInExp t (EFLam tau body) = EFLam (substTyp t tau) (substTypInExp t body)
substTypInExp t (EFApp fun arg) = EFApp (substTypInExp t fun) (substTypInExp t arg)
substTypInExp t (ETApp exp tau) = ETApp (substTypInExp t exp) (substTyp t tau)
substTypInExp _ e = e
