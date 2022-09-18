module Syntax (
      Name
    , Literal(..)
    , Prim(..)
    , Compare(..)
    , Def(..)
    , Expr(..)
    , arity
) where
import Data.Text as T

type Name = T.Text

data Literal
    = LInt Int
    | LReal Double
    | LBool Bool
    | LChar Char
    | LUnit
    deriving (Eq, Ord)

data Compare = Eq | Ne | Gr | Ls | Ge | Le
    deriving (Eq, Ord)

data Prim
    = IAdd
    | ISub
    | INeg
    -- interger comparing primitives
    | ICmp Compare
    -- boolean primitives
    | BNot
    | BAnd
    | BOr
    | BXor
    -- IO primitives
    | IOReadInt
    | IOWriteInt
    deriving (Eq, Ord)

data Def a = Def 
    { func :: a
    , args :: [a]
    , body :: Expr a
    } deriving (Eq, Ord)

data Expr a
    = ELit Literal
    | EVar a
    | ELam [a] (Expr a)
    | EApp (Expr a) [Expr a]
    | EOpr Prim [Expr a]
    | ELet a (Expr a) (Expr a)
    | EFix [Def a] (Expr a)
    | EIfte (Expr a) (Expr a) (Expr a)
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
arity (ICmp _) = 2
arity BNot = 1
arity BAnd = 2
arity BOr = 2
arity BXor = 2
arity IOReadInt = 0
arity IOWriteInt = 1