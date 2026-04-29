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

shiftTyp :: Int -> Int -> Typ -> Typ
shiftTyp = _

substTyp :: Typ -> Typ -> Typ
substTyp = _

shiftExp :: Int -> Int -> Exp -> Exp
shiftExp = _

substExp :: Exp -> Exp -> Exp
substExp = _

substTypInExp :: Typ -> Exp -> Exp
substTypInExp = _
