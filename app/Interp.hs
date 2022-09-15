{-# OPTIONS_GHC -Wno-typed-holes #-}
module Interp where
import Data.Map as M
import Data.List as L
import Syntax
import Data.Maybe (fromMaybe, fromJust)
import Text.Read (readMaybe)
import Control.Monad

data Value a
    = VLit Literal
    | VClos (Env a) [a] (Expr a)

type Env a = M.Map a (Value a)

interp :: Ord a => Expr a -> IO (Value a)
interp = interp' M.empty

interp' :: Ord a => Env a -> Expr a -> IO (Value a)
interp' env (EVar x) =
    return $ fromJust $ M.lookup x env
interp' env (ELit c) =
    return $ VLit c
interp' env (ELam x e) =
    return $ VClos env x e
interp' env (EApp es) = do
    (f:xs) <- mapM (interp' env) es
    case f of
        (VLit _) -> error "literal can't be function!"
        (VClos env' ys e) ->
            if length xs == length ys
            then interp' (M.fromAscList (zip ys xs) `M.union` env') e
            else error "argument number doesn't match"
interp' env (ELet x e1 e2) = do
    e1' <- interp' env e1
    interp' (M.insert x e1' env) e2
interp' env (EOpr prim args) = do
    args' <- mapM (interp' env) args
    return $ interpOpr prim args'
interp' env (EEff prim args) = do
    args' <- mapM (interp' env) args
    interpEff prim args'

interpOpr :: Prim -> [Value a] -> Value a
interpOpr INeg [VLit (LInt n)] = VLit $ LInt (- n)
interpOpr IAdd [VLit (LInt x), VLit (LInt y)] = VLit $ LInt (x + y)
interpOpr ISub [VLit (LInt x), VLit (LInt y)] = VLit $ LInt (x - y)
interpOpr _ _ = error "interpOpr failed!"

interpEff :: EffPrim -> [Value a] -> IO (Value a)
interpEff IOReadInt [] = do
    s <- getLine
    return $ VLit $ LInt $ fromMaybe 0 (readMaybe s :: Maybe Int)
interpEff IOWriteInt [VLit (LInt x)] = do
    print x
    return $ VLit LUnit
interpEff _  _ = error "interpEff failed!"


