module Desugar (
  desugar
) where

import qualified Kx.Abs as A

import Types
import BookKeeping
import Data.List (elemIndex)

desugar :: A.Exp -> Exp
desugar = desugarExp $ emptyContext

type SugarCtx = Context Ident

bindTyp :: Ident -> Context Ident -> Context Ident
bindTyp = bindTyp'

bindTerm :: Ident -> Context Ident -> Context Ident
bindTerm = bindTerm'

desugarTyp :: SugarCtx -> A.Typ -> Typ
desugarTyp s (A.TVar t) = case elemIndex t (boundTyps s) of
  Just i -> (TVar i)
  Nothing -> (TFree t)
desugarTyp s (A.TArr tau tau') = TArr (desugarTyp s tau) (desugarTyp s tau')
desugarTyp s (A.TAll t tau)  = TAll (desugarTyp (bindTyp t s) tau)

desugarExp :: SugarCtx -> A.Exp -> Exp
desugarExp s (A.EVar x) = case elemIndex x (boundTerms s) of
  Just i -> (EVar i)
  Nothing -> (EFree x)
desugarExp s (A.EFApp f e) =
  EFApp (desugarExp s f) (desugarExp s e)
desugarExp s (A.EFLam x tau e) =
  let tau' = desugarTyp s tau
  in EFLam tau' (desugarExp (bindTerm x s) e)
desugarExp s (A.ETApp e tau) =
  ETApp (desugarExp s e) (desugarTyp s tau)
desugarExp s (A.ETLam t e) =
  ETLam (desugarExp (bindTyp t s) e)
