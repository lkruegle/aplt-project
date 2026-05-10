module Desugar
  ( desugar,
  )
where

import BookKeeping
import Data.List (elemIndex)
import qualified Kx.Abs as A
import Types

-- TODO: Remove the need for

-- | Entrypoint to the desugaring step
--
-- Converts The abstract syntax defined by the grammar into the core
-- language, desugaring any syntax-only features.
desugar :: A.Exp -> Exp
desugar = desugarExp emptyContext

-- | Desugaring context used for managing variable binding
type SugarCtx = Context Ident

-- | Desugar a type
desugarTyp :: SugarCtx -> A.Typ -> Typ
desugarTyp _ A.TNat = TNat
desugarTyp s (A.TVar t) = case elemIndex t (boundTyps s) of
  Just i -> TVar i
  Nothing -> TFree t
desugarTyp s (A.TArr tau tau') = TArr (desugarTyp s tau) (desugarTyp s tau')
desugarTyp _ _ = undefined

-- desugarTyp s (A.TAll t tau)  = TAll (desugarTyp (bindTyp t s) tau)
-- desugarTyp s (A.TProd taus) = TProd (map (desugarTyp s) taus)
-- desugarTyp s (A.TSum taus) = TSum (map (desugarTyp s) taus)

-- | Desugar an expression
desugarExp :: SugarCtx -> A.Exp -> Exp
desugarExp s (A.EVar x) = case elemIndex x (boundTerms s) of
  Just i -> EVar i x
  Nothing -> EFree x
desugarExp _ A.EZero = EZero
desugarExp s (A.ESucc e) =
  ESucc $ desugarExp s e
desugarExp s (A.EFApp f e) =
  EFApp (desugarExp s f) (desugarExp s e)
desugarExp s (A.EFLam x tau e) =
  let tau' = desugarTyp s tau
   in EFLam x tau' (desugarExp (bindTerm x s) e)
desugarExp s (A.ELet x t e1 e2) =
  desugarExp s $ A.EFApp (A.EFLam x t e2) e1
desugarExp _ _ = undefined

-- desugarExp s (A.ETApp e tau) =
--   ETApp (desugarExp s e) (desugarTyp s tau)
-- desugarExp s (A.ETLam t e) =
--   ETLam (desugarExp (bindTyp t s) e)
-- desugarExp s (A.ETupl es) = ETupl (map (desugarExp s) es)
-- desugarExp s (A.EProj e i) = EProj (desugarExp s e) (fromIntegral i)
-- desugarExp s (A.ECase e es) = ECase (desugarExp s e) (map go es)
--   where
--     go (A.Match i e) = desugarExp (bindTerm i s) e
-- desugarExp s (A.EInj i e) = EInj (fromIntegral i) (desugarExp s e)
