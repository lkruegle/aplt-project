module TypeChecker (typecheck) where

import qualified Kx.Abs as Abs

import Types(Ctx, Typ(..), Exp (..), subTypInTyp)

import qualified Data.Map as Map

typecheck :: Abs.Exp -> Either String (Exp, Typ)
typecheck e = infer Map.empty e 

infer :: Ctx -> Abs.Exp -> Either String (Exp, Typ)
infer ctx (Abs.EVar (Abs.Ident x)) = case Map.lookup x ctx of
    Just tau  -> Right(EVar x, tau)
    Nothing -> Left "Variable do not exisit in current context"
infer ctx (Abs.EFLam (Abs.Ident x) tau1 e) = do
    tau1' <- inferT ctx tau1
    (e', tau2') <- infer (Map.insert x tau1' ctx) e
    Right (EFLam x tau1' e', TArr tau1' tau2')
infer ctx (Abs.EFApl f x) = do
    (x', tau1') <- infer ctx x
    case infer ctx f of
        Right (f', TArr tau2' tau') | tau1' == tau2' -> Right (f', tau')
                            | otherwise -> Left " "
        _ -> Left "The type of a function aplication must be a arrow"
infer ctx (Abs.ETLam (Abs.Ident t) e) = do -- Needs better names?
    (e', tau') <- infer (Map.insert t (TVar t) ctx) e
    Right (ETLam t e', TAll t tau')
infer ctx (Abs.ETApp f tau) = do
    tau <- inferT ctx tau
    case infer ctx f of
        Right (f', (TAll t tau')) -> Right (ETApp f' tau, subTypInTyp ctx tau t tau')
        _ -> Left "Not a type lambda"

inferT :: Ctx -> Abs.Typ -> Either String Typ
inferT ctx (Abs.TVar (Abs.Ident t)) = case Map.lookup t ctx of
    Just (TVar t) -> Right (TVar t) -- May need somthing that separate typevariabels from variables in the context
    Nothing -> Left "Type variable do not exisit in current context"
inferT ctx (Abs.TArr tau1 tau2) = do
    tau1' <- inferT ctx tau1
    tau2' <- inferT ctx tau2
    (Right (TArr tau1' tau2'))
inferT ctx (Abs.TAll (Abs.Ident t) tau) = do
    tau' <- inferT ctx tau
    (Right (TAll t tau'))