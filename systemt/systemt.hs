import qualified Data.Map as M

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

data Type = Num | Fun Type Type
  deriving (Eq, Show)

type Name = String

data Value = VNat Nat | VClo Env Type Name Exp
  deriving (Show)

data Exp
  = V Name
  | Z
  | S Exp
  | R Exp Exp Exp
  | L Type Name Exp
  | A Exp Exp
  deriving (Show)

type Env = M.Map Name Value

type Cxt = M.Map Name Type

eval' :: Exp -> Value
eval' e = eval M.empty e

eval :: Env -> Exp -> Value
eval env = \case
  Z -> VNat Zero
  S e -> let (VNat n) = eval env e in VNat (Succ n)
  V name -> case M.lookup name env of
    Just value -> value -- guaranteed by type checker
  R Z b r -> eval env b
  R (S n) b r -> eval env (A (A r n) (R n b r))
  L t v e -> VClo env t v e
  A f a ->
    let (VClo clo _ x e) = eval env f
     in let v = eval env a
         in let clo' = M.insert x v clo
             in eval clo' e

infer' :: Exp -> Maybe Type
infer' e = infer M.empty e

infer :: Cxt -> Exp -> Maybe Type
infer cxt = \case
  Z -> return Num
  S e -> if check cxt Num e then return Num else Nothing
  V name -> M.lookup name cxt
  L at x e -> do
    let cxt' = M.insert x at cxt
    rt <- infer cxt' e
    return $ Fun at rt
  R n b r ->
    if check cxt Num n
      then do
        t <- infer cxt b
        case infer cxt r of
          Just (Fun Num (Fun t' t'')) ->
            if t == t' && t == t'' then return t else Nothing
          _ -> Nothing
      else Nothing
  A f a -> do
    Fun at rt <- infer cxt f
    if check cxt at a then return rt else Nothing

check' :: Type -> Exp -> Bool
check' t e = check M.empty t e

check :: Cxt -> Type -> Exp -> Bool
check cxt t e = case infer cxt e of
  Just t' -> t' == t
  Nothing -> False

run :: Exp -> Maybe Value
run e = do
  infer' e
  return $ eval' e
