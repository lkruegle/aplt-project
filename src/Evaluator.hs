module Evaluator where

import qualified Data.Map as Map
import qualified Kx.Abs as Abs
import Kx.Abs(Typ, Typ(..), Ident)

import Types(Typed(..), Exp (..), Val(..))

type Env = (Map.Map Ident (Typed Exp), Map.Map Ident Typ)

bindVar:: Ident -> (Typed Exp) -> Env -> Env 
bindVar x e (xs, ts) = (Map.insert x e xs, ts) 
bindTyp:: Ident -> Typ -> Env -> Env
bindTyp t tau (xs, ts) = (xs, Map.insert t tau ts) 

getVar:: Ident -> Env -> (Typed Exp)
getVar x (xs, _) = case Map.lookup x xs of
    Just e -> e
    Nothing -> error  ("variable " ++ show x ++ " not found")
getTyp:: Ident -> Env -> Typ
getTyp t (_, ts) = case Map.lookup t ts of
    Just tau -> tau
    Nothing -> error  ("type variable " ++ show t ++ " not found")

eval' :: (Typed Exp) -> (Typed Val)
eval' e = eval (Map.empty, Map.empty) e

eval :: Env -> (Typed Exp) -> (Typed Val)
eval env (Typed (EVar x) tau) = eval env (getVar x env)
--(16.3a)
eval env (Typed (EFLam x _ e) tau) = Typed (VFLam (\e' -> eval (bindVar x e' env) e)) tau
--(16.3b)
eval env (Typed (ETLam t e) tau) = Typed (VTLam (\tau -> eval (bindTyp t tau env) e)) tau
--(16.3c)
eval env (Typed (EFApp (Typed (EFLam x _ e) _) e2) _) = eval (bindVar x e2 env) e 
--(16.3d)
eval env (Typed (EFApp e1 e2) _) = case eval env e1 of
    Typed (VFLam e1') _ -> e1' e2
    _ -> error "trying to do function application on a non function lambda expression"
--(16.3f)
eval env (Typed (ETApp (Typed (ETLam t e) _) tau) _) = eval (bindTyp t tau env) e
--(16.3g)
eval env (Typed (ETApp e tau) _) = case eval env e of
    Typed (VTLam e') _ -> e' tau
    _ -> error "trying to do type application on a non type lambda expression"
