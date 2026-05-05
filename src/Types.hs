module Types (
  module Types,
  Ident(..)
) where
import Kx.Abs(Ident(..))

data Typ
  = TNat
  | TVar Int
  | TFree Ident
  | TArr Typ Typ
  | TAll Typ
  deriving (Show, Eq)

data Exp
  = EZero
  | ESucc Exp
  | EVar Int
  | EFree Ident
  | EFLam Typ Exp
  | EFApp Exp Exp
  | ETLam Exp
  | ETApp Exp Typ
  deriving (Show, Eq)

data Val
  = VNat Int
  | VLam Typ Exp
  | VTLam Exp
  deriving (Show, Eq)
