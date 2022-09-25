module TypeCheck where
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Syntax
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS
import Data.Maybe
import Data.IntMap.Merge.Lazy (merge)

data InferState = InferState
    { fresh :: Int
    , assign :: M.Map Name (MonoType Name)
    }

emptyState :: InferState
emptyState = InferState
    { fresh = 0
    , assign = M.empty
    }

type InferEnv = M.Map Name (PolyType Name)

data InferError = InferError

type Infer =  RWST InferEnv [InferError] InferState (Either InferError)

newvar :: Infer (MonoType Name)
newvar = do
    n <- gets fresh
    modify $ \s -> s{ fresh = n + 1 }
    -- we assume that user defined typevar never starts with '#'
    return $ TVar $ "#" ++ show n

unify :: MonoType Name -> MonoType Name -> Infer ()
unify (TLit a) (TLit b)
    | a == b = return ()
    | otherwise = throwError InferError
unify (TFun xs x) (TFun ys y)
    | L.length xs == L.length ys = do
        mapM_ (uncurry unify) (L.zip xs ys)
        unify x y
    | otherwise = throwError InferError
unify (TApp x xs) (TApp y ys)
    | L.length xs == L.length ys = do
        mapM_ (uncurry unify) (L.zip xs ys)
        unify x y
    | otherwise = throwError InferError
unify (TVar x) (TVar y)
    | x == y = return ()
unify (TVar x) ty = do
    a <- gets assign
    case M.lookup x a of
        Just ty' -> unify ty ty'
        Nothing -> modify $ \s -> s{ assign = M.insert x ty a }
unify ty (TVar x) =
    unify (TVar x) ty
unify _ _ = throwError InferError

subst :: M.Map Name (MonoType Name) -> MonoType Name -> MonoType Name
subst s t@(TLit _) = t
subst s t@(TVar x) =
    -- may be captrued by gamma?
    fromMaybe t (M.lookup x s) 
subst s (TFun xs x) =
    TFun (fmap (subst s) xs) (subst s x)
subst s (TApp x xs) =
    TApp (subst s x) (fmap (subst s) xs)

mergeType :: MonoType Name -> Infer (MonoType Name)
mergeType t@(TLit _) = return t
mergeType t@(TVar x) = do
    a <- gets assign
    case M.lookup x a of
        Just ty -> mergeType ty
        Nothing -> return t
mergeType (TFun xs x) = do
    xs' <- mapM mergeType xs
    x' <- mergeType x
    return $ TFun xs' x'
mergeType (TApp x xs) = do
    x' <- mergeType x
    xs' <- mapM mergeType xs
    return $ TApp x' xs'

freevar :: MonoType Name -> Infer (S.Set Name)
freevar (TLit _) = return S.empty
freevar (TVar x) = do
    a <- asks $ M.lookup x
    -- if it is in the gamma, it's not free 
    case a of
        Just _ -> return S.empty
        Nothing -> return $ S.singleton x
freevar (TFun xs x) = do
    xs' <- mapM freevar xs
    x' <- freevar x
    return $ S.unions (x':xs')
freevar (TApp x xs) = do
    x' <- freevar x
    xs' <- mapM freevar xs
    return $ S.unions (x':xs')

instantiate :: PolyType Name -> Infer (MonoType Name)
instantiate (PolyType xs ty) = do
    vars <- replicateM (L.length xs) newvar 
    let s = M.fromList (L.zip xs vars)
    return $ subst s ty

generalize :: MonoType Name -> Infer (PolyType Name)
generalize ty = do
    ty' <- mergeType ty
    xs <- freevar ty'
    return $ PolyType (S.toAscList xs) ty'

typeInferLiteral :: Literal -> LitType
typeInferLiteral (LInt _) = LitInt
typeInferLiteral (LReal _) = LitReal
typeInferLiteral (LBool _) = LitBool
typeInferLiteral (LChar _) = LitChar
typeInferLiteral LUnit = LitUnit


binopType :: LitType -> LitType -> LitType -> MonoType a
binopType ty1 ty2 ty3 = TFun [TLit ty1, TLit ty2] (TLit ty3)

typeInferPrim :: Prim -> MonoType Name
typeInferPrim IAdd =
    binopType LitInt LitInt LitInt
typeInferPrim ISub =
    binopType LitInt LitInt LitInt
-- todo: more primitives
typeInferPrim _ = undefined

typeInfer :: Expr Name -> Infer (MonoType Name)
typeInfer (ELit lit) =
    return $ TLit $ typeInferLiteral lit
typeInfer (EVar x) = do
    pty <-  asks $ M.lookup x
    case pty of
        Just pty -> instantiate pty
        Nothing -> throwError InferError
typeInfer (EFun xs x) = do
    ys <- replicateM (L.length xs) newvar
    let s = M.fromList $ L.zip xs (fmap (PolyType []) ys)
    res <- local (M.union s) (typeInfer x)
    return $ TFun ys res
typeInfer (EApp func args) = do
    func' <- typeInfer func
    args' <- mapM typeInfer args
    res <- newvar
    unify func' (TFun args' res)
    return res
typeInfer (ELet x e1 e2) = do
    e1' <- typeInfer e1
    pty <- generalize e1'
    local (M.insert x pty) (typeInfer e2)
typeInfer (EIfte cond trbr flbr) = do
    cond' <- typeInfer cond
    unify cond' (TLit LitBool)
    trbr' <- typeInfer trbr
    flbr' <- typeInfer flbr
    unify trbr' flbr'
    return trbr'
typeInfer (EOpr op args) = do
    let op' = typeInferPrim op
    args' <- mapM typeInfer args
    res <- newvar
    unify op' (TFun args' res)
    return res
typeInfer (EFix defs body) =
    undefined
typeInfer (EAnno expr ty) =
    undefined

typeCheck :: Expr Name -> MonoType Name -> Infer (MonoType Name)
typeCheck _ _ = undefined


typeInferTop :: Expr Name -> Either [InferError] (MonoType Name)
typeInferTop expr = 
    case evalRWST (typeInfer expr) M.empty emptyState of
        Left err -> Left [err]
        Right (ty,err) -> Right ty




{-
insertEnv :: Name -> PolyType Name -> Infer ()
insertEnv x pty = do
    ctx <- gets context
    modify $ \s -> s{context = M.insert x (Just pty) ctx}





-- typeInfer ctx (ELam xs e) = M.lookup x ctx
typeInfer expr = undefined


typeCheck :: Expr Name -> MonoType Name -> Infer 
-}