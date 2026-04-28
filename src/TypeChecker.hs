module TypeChecker (typecheck) where

import qualified Kx.Abs as Abs
import Kx.Abs(Typ, Typ(..), Ident)

import Types(Typed(..), Exp (..))

import qualified Data.Map as Map

type Ctx = (Map.Map Ident Typ, Map.Map Ident Typ)

bindVar:: Ident -> Typ -> Ctx -> Ctx 
bindVar x e (xs, ts) = (Map.insert x e xs, ts) 
bindTyp:: Ident -> Typ -> Ctx -> Ctx
bindTyp t tau (xs, ts) = (xs, Map.insert t tau ts) 

getVar:: Ident -> Ctx -> Maybe Typ
getVar x (xs, _) = Map.lookup x xs
getTyp:: Ident -> Ctx -> Maybe Typ
getTyp t (_, ts) = Map.lookup t ts

typecheck :: Abs.Exp -> Either String (Typed Exp)
typecheck e = infer (Map.empty, Map.empty) e 

inferT :: Ctx -> Abs.Typ -> Either String Typ
--16.1a
inferT ctx (Abs.TVar  t) = case getTyp t ctx of
    Just tau -> inferT ctx tau
    Nothing -> Left "Type variable do not exisit in current context"
--16.1b
inferT ctx (Abs.TArr tau1 tau2) = do
    tau1' <- inferT ctx tau1
    tau2' <- inferT ctx tau2
    (Right (TArr tau1' tau2'))
--16.1c
inferT ctx (Abs.TAll t tau) = do
    tau' <- inferT ctx tau
    (Right (TAll t tau'))

infer :: Ctx -> Abs.Exp -> Either String (Typed Exp)
--16.2a
infer ctx (Abs.EVar x) = case getVar x ctx of
    Just tau  -> Right (Typed (EVar x) tau)
    Nothing -> Left "Variable do not exisit in current context"
--16.2b
infer ctx (Abs.EFLam x tau1 e) = do
    tau1' <- inferT ctx tau1
    e'@(Typed _ tau2') <- infer (bindVar x tau1' ctx) e
    Right (Typed (EFLam x tau1' e') (TArr tau1' tau2'))
--16.2c
infer ctx (Abs.EFApp f x) = do
    (Typed _ tau1') <- infer ctx x
    case infer ctx f of
        Right (Typed f' (TArr tau2' tau')) | tau1' == tau2' -> Right (Typed f' tau')
                            | otherwise -> Left " "
        _ -> Left "The type of a function aplication must be a arrow"
--16.2d
infer ctx (Abs.ETLam t e) = do -- Needs better names?
    e'@(Typed _ tau') <- infer (bindTyp t (TVar t) ctx) e
    Right (Typed (ETLam t e') (TAll t tau'))
--16.2e
infer ctx (Abs.ETApp f tau) = do
    tau <- inferT ctx tau
    case infer ctx f of
        Right f'@(Typed _ (TAll t tau')) -> do
            itau <- inferT (bindTyp t tau ctx) tau'
            Right (Typed (ETApp f' tau) itau)
        _ -> Left "Not a type lambda"