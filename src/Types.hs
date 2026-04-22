module Types where
import qualified Data.Map as Map

data Typ 
    = TVar String 
    | TArr Typ Typ 
    | TAll String Typ
    deriving (Eq, Show)

data Exp
    = EVar String
    | EFLam String Typ Exp
    | EFApl Exp Exp
    | ETLam String Exp
    | ETApp Exp Typ
    deriving (Show)

data Val 
    = VFLam String Typ Exp
    | VTLam String Exp
    deriving (Show)

type Ctx = Map.Map String Typ

type Env = Map.Map String Exp

--[tau/ t] tau'
-- substitute each free instance of t with tau in tau'
subTypInTyp :: Ctx -> Typ -> String -> Typ -> Typ
subTypInTyp ctx tau t (TVar t') 
    | t == t' && Map.notMember t ctx = tau 
    | otherwise = (TVar t') -- Not free
subTypInTyp ctx tau t (TArr tau1' tau2') = TArr (subTypInTyp ctx tau t tau1') ( subTypInTyp ctx tau t tau2') 
subTypInTyp ctx tau t (TAll t' tau')  = TAll t' (subTypInTyp (Map.insert t (TVar t) ctx) tau t tau')