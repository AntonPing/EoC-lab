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
    , Type(..)
    , Value(..)
    , InferError(..)
    , arity
    , eraseType
) where
import qualified Data.Text as T
import qualified Data.Map as M

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
arity Move = 1

data Def ty = Def 
    { func :: Name
    , args :: [(Name, ty)]
    , body :: Expr ty
    , typ :: ty
    } deriving (Eq, Ord)

data Expr ty
    = ELit Literal
    | EVar Name
    | EFun [(Name, ty)] (Expr ty)
    | EApp (Expr ty) [Expr ty]
    | EOpr Prim [Expr ty]
    | ELet Name ty (Expr ty) (Expr ty)
    | EFix [Def ty] (Expr ty)
    | EIfte (Expr ty) (Expr ty) (Expr ty)
    | EAnno (Expr ty) ty
    deriving (Eq, Ord)

eraseType :: Expr ty -> Expr ()
eraseType (ELit lit) = ELit lit
eraseType (EVar x) = EVar x
eraseType (EFun xs e) = 
    EFun (fmap (\(x,t)->(x,())) xs) (eraseType e)
eraseType (EApp x xs) =
    EApp (eraseType x) (fmap eraseType xs)
eraseType (EOpr op xs) =
    EOpr op (fmap eraseType xs)
eraseType (ELet x ty e1 e2) =
    ELet x () (eraseType e1) (eraseType e2)
eraseType (EFix defs body) =
    EFix (fmap eraseTypeDef defs) (eraseType body)
eraseType (EIfte cond trbr flbr) =
    EIfte (eraseType cond) (eraseType trbr) (eraseType flbr)
eraseType (EAnno e ty) =
    EAnno (eraseType e) ()

eraseTypeDef :: Def ty -> Def ()
eraseTypeDef Def{ func, args, body, typ} =
    Def { func
        , args = fmap (\(a,b) -> (a,())) args
        , body = eraseType body
        , typ = ()
        }

data LitType
    = LitInt
    | LitReal
    | LitBool
    | LitChar
    | LitUnit
    | LitVoid
    deriving (Eq, Ord)

data MonoType
    = TLit LitType
    | TVar Name
    | TCon Name
    | TFun [MonoType] MonoType
    | TApp MonoType [MonoType]
    deriving (Eq, Ord)

data PolyType = PolyType [Name] MonoType

unwrapPoly :: PolyType -> MonoType
unwrapPoly (PolyType [] ty) = ty
unwrapPoly (PolyType xs ty) = undefined

data Type
    = Mono MonoType
    | Poly PolyType

data Kind = 
      Star
    | KArr Kind Kind
    deriving (Eq, Ord)


type Env = M.Map Name Value

data Value
    = VLit Literal
    | VClos Env [Name] (Expr ())
    -- VFix is almost like VClos
    -- but each time you evaluate it, you inject the defs into the old env
    | VFix Env [Name] (Expr ()) (M.Map Name (Def ()))



data InferError
    = CantUnify MonoType MonoType
    | VarNotInScope Name
