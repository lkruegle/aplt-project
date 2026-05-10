module TypeChecker (typecheck, Inferred(..)) where

import Types


data Inferred (γ :: [Typ]) where
  Inferred :: γ ⊢ τ -> Inferred γ

-- | Entrypoint for the typechecker
--
-- Given a desugared expression, infer the type the expression will evaluate
-- to.
typecheck :: Exp -> M (Inferred '[])
typecheck = undefined

type M a = Either String a
