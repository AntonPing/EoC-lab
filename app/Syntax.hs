module Syntax (
      Name
    , Literal(..)
    , Prim(..)
    , Compare(..)
    , Def(..)
    , Expr(..)
    , LitType(..)
    , MonoType(..)
    , PolyType(..)
    , arity
) where
import Data.Text as T

type Name = String

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
    -- just move
    | Move
    deriving (Eq, Ord)

data Def a = Def 
    { func :: a
    , args :: [a]
    , body :: Expr a
    } deriving (Eq, Ord)

data Expr a
    = ELit Literal
    | EVar a
    | EFun [a] (Expr a)
    | EApp (Expr a) [Expr a]
    | EOpr Prim [Expr a]
    | ELet a (Expr a) (Expr a)
    | EFix [Def a] (Expr a)
    | EIfte (Expr a) (Expr a) (Expr a)
    | EAnno (Expr a) (MonoType a)
    deriving (Eq, Ord)

data LitType
    = LitInt
    | LitReal
    | LitBool
    | LitChar
    | LitUnit
    | LitVoid
    deriving (Eq, Ord)

data MonoType a
    = TLit LitType
    | TVar a
    | TFun [MonoType a] (MonoType a)
    | TApp (MonoType a) [MonoType a]
    -- only used in HM type infer
    -- | TTemp Int
    deriving (Eq, Ord)

data PolyType a = PolyType [a] (MonoType a) 

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