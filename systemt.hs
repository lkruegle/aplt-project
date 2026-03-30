import qualified Data.Map as M

data Nat = Zero | Succ Nat

data Type = NatT | ArrT Type Type

type Var = String
data Value = VNat Nat | VLam Type Var Exp

data Exp
  = V Var
  | Z
  | S Exp
  | R Exp Exp Exp
  | L Type Var Exp
  | A Exp Exp

-- prg = S $ S Z
-- eval prg -- 2

eval :: Env -> Exp -> Value
eval env = \case
  Z -> Zero
  S e -> Succ $ eval env e
  V name -> case M.lookup name env of
    Just value -> value -- guaranteed by type checker
  R n b r -> undefined
  L t v e -> Lam t v e
  A f a ->
    let (Lam _ x e) = eval env f in
    let v = eval env a in
    let env' = Map.insert x v env in
    eval env' e

type Cxt = M.Map Var Type
type Env = M.Map Var Value

infer :: Cxt -> Exp -> Maybe Type
infer = undefined

check :: Cxt -> Exp -> Type -> Bool
check = undefined
