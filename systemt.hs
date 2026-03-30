import qualified Data.Map as M

data Type = NatT | ArrT Type Type

type Var = String
data Value = Num Nat | Lam Var Type Exp

data Exp
  = Var Var
  | Z
  | S Exp
  | Rec Exp Exp Exp
  | Lam Type Var Exp
  | Ap Exp Exp

type Cxt = M.Map Var Type
type Env = M.Map Var Value

infer :: Cxt -> Exp -> Maybe Type
infer = undefined

check :: Cxt -> Exp -> Type -> Bool
check = undefined

eval :: Env -> Exp -> Value
eval = undefined
