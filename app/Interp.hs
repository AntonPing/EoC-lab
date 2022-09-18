{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}

module Interp where
import Data.Map as M
import Data.List as L
import Syntax
import Data.Maybe (fromMaybe, fromJust)
import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Except

data Value a
    = VLit Literal
    | VClos (Env a) [a] (Expr a)
    -- VFix is almost like VClos
    -- but each time you evaluate it, you inject the defs into the old env
    | VFix (Env a) [a] (Expr a) (M.Map a (Def a))

data InterpError
    = UnboundVar String
    | NotAFunc String
    | DiffArgNum String
    | OprFailed String
    | CondNotBool String
    deriving (Show)

type Env a = M.Map a (Value a)

interp :: Ord a => Expr a -> IO (Either InterpError (Value a))
interp expr = runExceptT $ interp' M.empty expr

interp' :: Ord a => Env a -> Expr a -> ExceptT InterpError IO (Value a)
interp' env (EVar x) =
    case M.lookup x env of
        Just val -> return val
        Nothing -> throwError $ UnboundVar "unbounded variable"
interp' env (ELit c) =
    return $ VLit c
interp' env (ELam xs body) =
    return $ VClos env xs body
interp' env (EApp func args) = do
    func' <- interp' env func
    case func' of
        (VLit _) ->
            throwError $ NotAFunc "literal can't be function!"
        (VClos env' xs e) ->
            if length xs == length args
            then do
                args' <- mapM (interp' env) args
                interp' (M.fromAscList (zip xs args') `M.union` env') e
            else throwError $ DiffArgNum "argument number doesn't match"
        (VFix env' xs e defs) ->
            if length xs == length args
            then do
                args' <- mapM (interp' env) args
                let env'' = injectFix defs env' -- inject defs into env'
                interp' (M.fromAscList (zip xs args') `M.union` env'') e
            else throwError $ DiffArgNum "argument number doesn't match"
interp' env (EOpr prim args) = do
    args' <- mapM (interp' env) args
    interpOpr prim args'
interp' env (ELet x e1 e2) = do
    e1' <- interp' env e1
    interp' (M.insert x e1' env) e2
interp' env (EFix defs e) =
    let assoc = fmap (\def -> (func def, def)) defs
        defs' = M.fromAscList assoc
    in interp' (injectFix defs' env) e
interp' env (EIfte cond trbr flbr) = do
    val <- interp' env cond
    case val of
        VLit (LBool p) ->
            interp' env (if p then trbr else flbr)
        _ -> throwError $ CondNotBool "the condition for if-then-else is not a boolean!"

interpOpr :: Prim -> [Value a] -> ExceptT InterpError IO (Value a)
interpOpr INeg [VLit (LInt n)] =
    return $ VLit $ LInt (- n)
interpOpr IAdd [VLit (LInt x), VLit (LInt y)] =
    return $ VLit $ LInt (x + y)
interpOpr ISub [VLit (LInt x), VLit (LInt y)] =
    return $ VLit $ LInt (x - y)
interpOpr IOReadInt [] = do
    s <- lift getLine
    case readMaybe s :: Maybe Int of
        Just x -> return $ VLit $ LInt x
        Nothing -> throwError $ OprFailed "failed to read an integer!"
interpOpr IOWriteInt [VLit (LInt x)] = do
    lift $ print x
    return $ VLit LUnit
interpOpr _  _ = throwError $ OprFailed "interpOpr failed!"

-- the injection function.  tricky, tricky...
injectFix :: Ord a => M.Map a (Def a) -> Env a -> Env a
injectFix defs env =
    let f Def{func,args,body} = VFix env args body defs
    in fmap f defs `M.union` env