module BookKeeping where

import Types

-- | Generic Context type
--
-- This type can be used by each pass to manage type and term
-- variable binding.
data Context a = Context {boundTyps :: [a], boundTerms :: [a]}

-- | Empty context constructor
emptyContext :: Context a
emptyContext = Context [] []

-- | Bind a term in the given context
bindTerm :: a -> Context a -> Context a
bindTerm x c = c {boundTerms = x : boundTerms c}

-- | Bind a typ in the given context
bindTyp :: a -> Context a -> Context a
bindTyp t c = c {boundTyps = t : boundTyps c}

-- | Lookup the given term's index in the current context
lookupTerm' :: Int -> Context a -> Maybe a
lookupTerm' = lookupC boundTerms

-- | Lookup the given typ's index in the current context
lookupTyp' :: Int -> Context a -> Maybe a
lookupTyp' = lookupC boundTyps

-- | Generic context lookup
lookupC :: (Context a -> [a]) -> Int -> Context a -> Maybe a
lookupC f i c = case drop i (f c) of
  (t : _) -> Just t
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
shiftTyp c (TProd taus) = TProd $ map (shiftTyp c) taus
shiftTyp c (TSum taus) = TSum $ map (shiftTyp c) taus
shiftTyp _ t = t

-- | Perform type variable substitution.
--
-- Replaces all type variables at index 0 with the given type and shifts the
-- remaining type variable's indices in response.
substTyp :: Typ -> Typ -> Typ
substTyp = sTyp 0
  where
    sTyp d s (TVar k)
      | k == d = sTypBy d 0 s
      | k > d = TVar (k - 1)
      | otherwise = TVar k
    sTyp d s (TArr a b) = TArr (sTyp d s a) (sTyp d s b)
    sTyp d s (TAll t) = TAll (sTyp (d + 1) s t)
    sTyp d s (TProd ts) = TProd (map (sTyp d s) ts)
    sTyp d s (TSum ts) = TSum (map (sTyp d s) ts)
    sTyp _ _ t = t
    -- \| Helper to do repeated shifts
    sTypBy 0 _ t = t
    sTypBy n c t = sTypBy (n - 1) c (shiftTyp c t)

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
shiftExp c (ETupl es) = ETupl (map (shiftExp c) es)
shiftExp c (EProj e i) = EProj (shiftExp c e) i
shiftExp c (ECase e es) = ECase (shiftExp c e) (map (shiftExp (c + 1)) es)
shiftExp c (EInj i e) = EInj i (shiftExp c e)
shiftExp _ e = e

-- | Perform term substitution.
--
-- Substitutes the given term for all variables with index 0 and shifts all
-- other variables down by 1.
substExp :: Exp -> Exp -> Exp
substExp = sExp 0
  where
    -- Do substitution while tracking the current depth at which substitution
    -- is happening. If a lambda occurs, shift all existing indices up by 1.
    sExp d e v@(EVar i)
      -- Shift the variables in e up by d to keep them valid
      | i == d = sExpN d 0 e
      | i > d = EVar (i - 1)
      | otherwise = v
    sExp d e (EFLam t body) = EFLam t $ sExp (d + 1) (shiftExp 0 e) body
    sExp d e (EFApp fun arg) = EFApp (sExp d e fun) (sExp d e arg)
    sExp d e (ETLam body) = ETLam $ sExp d e body
    sExp d e (ETApp body t) = ETApp (sExp d e body) t
    sExp d e (ESucc n) = ESucc (sExp d e n)
    sExp d e (ETupl es) = ETupl (map (sExp d e) es)
    sExp d e (EProj e' i) = EProj (sExp d e e') i
    sExp d e (ECase e' es) = ECase (sExp d e e') (map (sExp (d + 1) (shiftExp 0 e)) es)
    sExp d e (EInj i e') = EInj i (sExp d e e')
    sExp _ _ e' = e'
    -- Shift all variables in e with indices above c by n
    sExpN 0 _ e = e
    sExpN n c e = sExpN (n - 1) c (shiftExp c e)

-- | Perform type substitution across the given expression
--
-- Recurses the given expression, replacing any type variables with index 0
-- with the given type managing type binding and shifting.
substTypInExp :: Typ -> Exp -> Exp
substTypInExp t (ETLam body) = ETLam $ substTypInExp (shiftTyp 0 t) body
substTypInExp t (EFLam tau body) = EFLam (substTyp t tau) (substTypInExp t body)
substTypInExp t (EFApp fun arg) = EFApp (substTypInExp t fun) (substTypInExp t arg)
substTypInExp t (ETApp e tau) = ETApp (substTypInExp t e) (substTyp t tau)
substTypInExp t (ETupl es) = ETupl (map (substTypInExp t) es)
substTypInExp t (EProj e' i) = EProj (substTypInExp t e') i
substTypInExp t (EInj i e') = EInj i (substTypInExp t e')
substTypInExp t (ECase e es) = ECase (substTypInExp t e) (map (substTypInExp (shiftTyp 0 t)) es)
substTypInExp _ e = e
