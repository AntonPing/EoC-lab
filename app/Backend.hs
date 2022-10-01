{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}

module Backend where
import Syntax
import Control.Monad.State
import Control.Monad.Cont

data Atom
    = ALit Literal
    | AVar Name
    | ALabel Name

data MExpr
    = MLet Name Prim [Atom] MExpr
    | MCall Name Atom [Atom] MExpr
    | MRet Atom
    | MLabel Name MExpr

data Reg
    = Rax | Rcx | Rdx | Rbx
    | Rsp | Rbp | Rsi | Rdi
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15
    | Rip

data Oprand
    = Const Literal
    | InMem Reg Int
    | InReg Reg

data Opcode
    = Addq Oprand Oprand
    | Subq Oprand Oprand
    | Negq Oprand
    | Movq Oprand Oprand
    | Pushq Oprand
    | Popq Oprand
    | Callq  Oprand
    | Retq

type Block = [Opcode]

type NameGen = State Int


genName :: NameGen a
genName = undefined




{-
flatten' :: Expr a -> (Atom a -> MExpr a) -> NameGen (MExpr a)
flatten' (ELit lit) ctx =
    return $ ctx (ALit lit)
flatten' (EVar x) ctx =
    return $ ctx (AVar x)
flatten' (ELam xs e) ctx =
    undefined
flatten' (EApp xs e) ctx =
    undefined
flatten' (EOpr prim args) ctx = do
    name <- genName
    --args' <- forM args (const genName)
    args' <- mapM (flip flatten' AVar) args
    --args' <- foldM (\list argk -> flatten' argk (\x -> x:list)) [] args
    {-
    flatten' arg1 $ \arg1' ->
    flatten' arg2 $ \arg2' ->
    ......
    flatten' argn $ \argn' ->
    MLet name prim [AVar arg1', ..., AVar argn'] (ctx (AVar name))
    
    -}


    return $ MLet name prim (fmap AVar args') (ctx (AVar name))



flatten' (EFix defs e) ctx =
    undefined
-}

flatten :: Expr ty -> MExpr
flatten expr = 
    (flip evalState undefined . flip runContT (\a -> return $ MRet a))
    (flatten' expr)

flatten' ::  Expr a -> ContT MExpr NameGen Atom
flatten' (ELit lit) = return $ ALit lit
flatten' (EVar x) = return $ AVar x
flatten' (EFun xs e) = undefined
flatten' (EApp func args) = do
    func' <- flatten' func
    args' <- mapM flatten' args
    name <- lift genName
    let f expr = MCall name func' args' <$> expr;
    mapContT f (return $ AVar name)
flatten' (EOpr prim args) = do
    args' <- mapM flatten' args
    name <- lift genName
    let f expr = MLet name prim args' <$> expr;
    mapContT f (return $ AVar name)
flatten' (ELet x ty e1 e2) = do
    e1' <- flatten' e1
    let f expr = MLet x Move [e1'] <$> expr;
    mapContT f (flatten' e2)
flatten' (EFix defs e) =
    undefined
flatten' (EIfte cond trbr flbr) =
    undefined


