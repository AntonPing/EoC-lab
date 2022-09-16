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
    | IOReadInt
    | IOWriteInt
    deriving (Eq, Ord)

data Def a = Def 
    { name :: a
    , body :: Expr a
    } deriving (Eq, Ord)

data Expr a
    = ELit Literal
    | EVar a
    | ELam [a] (Expr a)
    | EApp (Expr a) [Expr a]
    | EOpr Prim [Expr a]
    | ELet [Def a] (Expr a)
    | ELetRec [Def a] (Expr a)
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

arity :: Prim -> Int
arity IAdd = 2
arity ISub = 2
arity INeg = 1
arity IOReadInt = 0
arity IOWriteInt = 1