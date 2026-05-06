module Types (
  module Types,
  Ident(..)
) where
import Kx.Abs(Ident(..))

data Typ
  = TNat
  | TVar Int
  | TFree Ident
  | TProd [Typ]
  | TArr Typ Typ
  | TAll Typ
  deriving (Show, Eq)

data Exp
  = EZero
  | ESucc Exp
  | EVar Int
  | EFree Ident
  | ETupl [Exp]
  | EProj Exp Int
  | EFLam Typ Exp
  | EFApp Exp Exp
  | ETLam Exp
  | ETApp Exp Typ
  deriving (Show, Eq)

data Val
  = VNat Int
  | VProd [Exp]
  | VLam Typ Exp
  | VTLam Exp
  deriving (Show, Eq)
