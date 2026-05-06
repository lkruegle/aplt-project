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
  | TSum [Typ]
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
  | ECase [Exp] Exp
  | EInj  Int Exp
  | EFLam Typ Exp
  | EFApp Exp Exp
  | ETLam Exp
  | ETApp Exp Typ
  deriving (Show, Eq)

data Val
  = VNat Int
  | VProd [Val]
  | VInj Int Val
  | VLam Typ Exp
  | VTLam Exp
  deriving (Show, Eq)
