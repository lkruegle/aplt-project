module Types where
import qualified Data.Map as Map
import qualified Kx.Abs as Abs
import Kx.Abs(Typ, Typ(..), Ident)
{-
data Typ 
    = TVar String 
    | TArr Typ Typ 
    | TAll String Typ
    deriving (Eq, Show)
-}
data Typed a = Typed a Typ

data Exp
    = EVar Ident
    | EFLam Ident Typ (Typed Exp)
    | EFApp (Typed Exp) (Typed Exp)
    | ETLam Ident (Typed Exp)
    | ETApp (Typed Exp) Typ



data Val 
    = VFLam (Typed Exp -> Typed Val)
    | VTLam (Typ -> Typed Val) 

instance Show (Typed Val) where
    show (Typed (VFLam _) t) = show t
    show (Typed (VTLam _) t) = show t