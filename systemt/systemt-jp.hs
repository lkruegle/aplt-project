data Exp
  = Var String
  | Zero
  | Succ Exp
  | Rec Exp Exp String String Exp
  | App Exp Exp
  | Lam String Exp

data Nat = Z | S Nat

data Val
   = NatVal Nat
   | FunVal (Val -> Val)

type Env = [(String, Val)]

eval :: Env -> Exp -> Val
eval rho (Var x) = case lookup x rho of
  Just v -> v
  Nothing -> error "panic: term is not scope-correct"
eval rho Zero = NatVal Z
eval rho (Succ e) = case eval rho e of
  NatVal v -> NatVal $ S v
  _ -> error "panic: term is not type-correct"
eval rho (App e1 e2) = case eval rho e1 of
  FunVal f -> f $ (eval rho e2)
  _ -> error "panic: term is not type-correct"
eval rho (Lam x e) = FunVal (\v -> eval ((x, v):rho) e)
eval rho (Rec e eBase x y eInd) = case eval rho e of
  NatVal v -> recursor v (eval rho eBase) (\n i -> eval ((x, NatVal n):(y, i):rho) iInd)
  _ -> error "panic: term is not type-correct"

recursor :: Nat -> t -> (Nat -> t -> t) -> t
recursor Z base ind = base
recursor (S n) base ind = ind n (recorsor n base ind)
