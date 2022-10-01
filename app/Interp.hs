module Interp where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Except
import Syntax


type Env = M.Map Name Value

data InterpError
    = UnboundVar String
    | NotAFunc String
    | DiffArgNum String
    | OprFailed String
    | CondNotBool String
    deriving (Show)

interp :: Expr () -> IO (Either InterpError Value)
interp expr = runExceptT $ interp' M.empty expr

interp' :: Env -> Expr () -> ExceptT InterpError IO Value
interp' env (EVar x) = do
    case M.lookup x env of
        Just val -> return val
        Nothing -> throwError $ UnboundVar "unbounded variable"
interp' env (ELit c) =
    return $ VLit c
interp' env (EFun xs body) =
    return $ VClos env (fmap fst xs) body
interp' env (EApp func args) = do
    func' <- interp' env func
    case func' of
        (VLit _) ->
            throwError $ NotAFunc "literal can't be function!"
        (VClos env' xs e) ->
            if length xs == length args
            then do
                args' <- mapM (interp' env) args
                interp' (M.fromList (zip xs args') `M.union` env') e
            else throwError $ DiffArgNum "argument number doesn't match"
        (VFix env' xs e defs) ->
            if length xs == length args
            then do
                args' <- mapM (interp' env) args
                let env'' = injectFix defs env' -- inject defs into env'
                interp' (M.fromList (zip xs args') `M.union` env'') e
            else throwError $ DiffArgNum "argument number doesn't match"
interp' env (EOpr prim args) = do
    args' <- mapM (interp' env) args
    interpOpr prim args'
interp' env (ELet x _ e1 e2) = do
    e1' <- interp' env e1
    interp' (M.insert x e1' env) e2
interp' env (EFix defs e) =
    let assoc = fmap (\def -> (func def, def)) defs
        defs' = M.fromList assoc
    in interp' (injectFix defs' env) e
interp' env (EIfte cond trbr flbr) = do
    val <- interp' env cond
    case val of
        VLit (LBool p) ->
            interp' env (if p then trbr else flbr)
        _ -> throwError $ CondNotBool "the condition for if-then-else is not a boolean!"
interp' env (EAnno expr ty) =
    interp' env expr

interpOpr :: Prim -> [Value] -> ExceptT InterpError IO Value
interpOpr INeg [VLit (LInt n)] =
    return $ VLit $ LInt (- n)
interpOpr IAdd [VLit (LInt x), VLit (LInt y)] =
    return $ VLit $ LInt (x + y)
interpOpr ISub [VLit (LInt x), VLit (LInt y)] =
    return $ VLit $ LInt (x - y)
interpOpr (ICmp cmp) [VLit (LInt x), VLit (LInt y)] =
    return $ VLit $ LBool (icompare cmp x y)
interpOpr BNot [VLit (LBool x)] =
    return $ VLit $ LBool (not x)
interpOpr BAnd [VLit (LBool x), VLit (LBool y)] =
    return $ VLit $ LBool (x && y)
interpOpr BOr [VLit (LBool x), VLit (LBool y)] =
    return $ VLit $ LBool (x || y)
interpOpr BXor [VLit (LBool x), VLit (LBool y)] =
    return $ VLit $ LBool (x /= y)
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
injectFix :: M.Map Name (Def ()) -> Env -> Env
injectFix defs env =
    let f Def{func,args,body,typ} = VFix env (fmap fst args) body defs
    in fmap f defs `M.union` env

icompare :: Compare -> Int -> Int -> Bool
icompare Eq = (==)
icompare Ne = (/=)
icompare Gr = (>)
icompare Ls = (<)
icompare Ge = (>=)
icompare Le = (<=)


