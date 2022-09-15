module Syntax where
import Data.Text as T

type Name = T.Text

data Literal
    = LInt Int
    | LReal Double
    | LBool Bool
    | LChar Char
    | LUnit
    deriving (Eq, Ord)

data Prim
    = IAdd
    | ISub
    | INeg
    deriving (Eq, Ord)
    
data EffPrim
    = IOReadInt
    | IOWriteInt
    deriving (Eq, Ord)

data Expr a
    = ELit Literal
    | EVar a
    | ELam [a] (Expr a)
    | EApp [Expr a]
    | EOpr Prim [Expr a]
    | EEff EffPrim [Expr a]
    | ELet a (Expr a) (Expr a)
    deriving (Eq, Ord)


data Type =
      TVar Name
    | TLam Name Kind Type
    | TApp Type Type
    | TLit Literal
    | TArr Type Type
    | TTup [Type]
    | TSum [Type]
    | TForall [Name] Type
    deriving (Eq, Ord)

data Kind = 
      Star
    | KArr Kind Kind
    deriving (Eq, Ord)