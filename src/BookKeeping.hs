module BookKeeping where

import Types

-- | Generic Context type
--
-- This type can be used by each pass to manage type and term
-- variable binding.
data Context a = Context { boundTyps :: [a], boundTerms :: [a] }

-- | Empty context constructor
emptyContext :: Context a
emptyContext = Context [] []

-- | Bind a term in the given context
bindTerm :: a -> Context a -> Context a
bindTerm x c = c { boundTerms = x : boundTerms c }

-- | Bind a typ in the given context
bindTyp :: a -> Context a -> Context a
bindTyp t c = c { boundTyps = t : boundTyps c }

-- | Lookup the given term's index in the current context
lookupTerm' :: Int -> Context a -> Maybe a
lookupTerm' = lookupC boundTerms

-- | Lookup the given typ's index in the current context
lookupTyp' :: Int -> Context a -> Maybe a
lookupTyp' = lookupC boundTyps

-- | Generic context lookup
lookupC :: (Context a -> [a]) -> Int -> Context a -> Maybe a
lookupC f i c = case drop i (f c) of
  (t:_) -> Just t
  [] -> Nothing

-- | Type variable shifting for managing de bruijn indices
-- Takes a cutoff and a Typ, any type variables within the type with indices
-- equal to or higher than the cutoff are shifted up by 1.
shiftTyp :: Int -> Typ -> Typ
shiftTyp c (TVar i)
  | i >= c = TVar $ i + 1
  | otherwise = TVar i
shiftTyp c (TArr arg ret) = TArr (shiftTyp c arg) (shiftTyp c ret)
shiftTyp c (TAll t) = TAll $ shiftTyp (c + 1) t
shiftTyp _ t = t

-- | Perform type variable substitution.
--
-- Replaces all type variables at index 0 with the given type and shifts the
-- remaining type variable's indices in response.
substTyp :: Typ -> Typ -> Typ
substTyp x (TVar 0) = x
substTyp x (TArr arg ret) = TArr (substTyp x arg) (substTyp x ret)
substTyp x (TAll t) = TAll (substTyp x t)
substTyp _ (TVar i) = TVar (i - 1) -- binding shifts vars down
substTyp _ t = t

-- | Perform term variable shifting of de bruijn indices.
-- Shifts all indices equal to or larger than the given cutoff
-- up by 1.
shiftExp :: Int -> Exp -> Exp
shiftExp c (EVar i)
  | i >= c = EVar $ i + 1
  | otherwise = EVar i
shiftExp c (EFLam t e) = EFLam t $ shiftExp (c + 1) e
shiftExp c (EFApp f a) = EFApp (shiftExp c f) (shiftExp c a)
shiftExp c (ETLam e) = ETLam $ shiftExp c e
shiftExp c (ETApp e t) = ETApp (shiftExp c e) t
shiftExp c (ESucc e) = ESucc (shiftExp c e)
shiftExp _ e = e

-- | Perform term substitution.
--
-- Substitutes the given term for all variables with index 0 and shifts all
-- other variables down by 1.
substExp :: Exp -> Exp -> Exp
substExp = sExp 0
  where
    -- | Do substitution while tracking the current depth at which substitution
    -- is happening. If a lambda occurs, shift all existing indices up by 1.
    sExp d e v@(EVar i)
      -- Shift the variables in e up by d to keep them valid
      | i == d = shiftExpN d 0 e
      | i > d = EVar (i - 1)
      | otherwise = v
    sExp d e (EFLam t body) = EFLam t $ sExp (d + 1) (shiftExp 0 e) body
    sExp d e (EFApp fun arg) = EFApp (sExp d e fun) (sExp d e arg)
    sExp d e (ETLam body) = ETLam $ sExp (d + 1) (shiftExp 0 e) body
    sExp d e (ETApp body t) = ETApp (sExp d e body) t
    sExp d e (ESucc n) = ESucc (sExp d e n)
    sExp _ _ e' = e'
    -- | Shift all variables in e with indices above c by n
    shiftExpN 0 _ e = e
    shiftExpN n c e = shiftExpN (n-1) c (shiftExp c e)

-- | Perform type substitution across the given expression
--
-- Recurses the given expression, replacing any type variables with index 0
-- with the given type managing type binding and shifting.
substTypInExp :: Typ -> Exp -> Exp
substTypInExp t (ETLam body) = ETLam $ substTypInExp (shiftTyp 0 t) body
substTypInExp t (EFLam tau body) = EFLam (substTyp t tau) (substTypInExp t body)
substTypInExp t (EFApp fun arg) = EFApp (substTypInExp t fun) (substTypInExp t arg)
substTypInExp t (ETApp e tau) = ETApp (substTypInExp t e) (substTyp t tau)
substTypInExp _ e = e
