{-# LANGUAGE LambdaCase #-}

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
import qualified Control.Arrow as Data.Bifunctor
import GHC.IO (unsafePerformIO)
import Printer
import Prettyprinter (Pretty(..))
import Prettyprinter.Render.Text (putDoc)
import Debug.Trace


data InferState = InferState
    { fresh :: Int
    , assign :: M.Map Name MonoType
    }

emptyState :: InferState
emptyState = InferState
    { fresh = 0
    , assign = M.empty
    }

type InferEnv = M.Map Name PolyType

type Infer =  RWST InferEnv [InferError] InferState (Either InferError)

newvar :: Infer MonoType
newvar = do
    n <- gets fresh
    modify $ \s -> s{ fresh = n + 1 }
    -- we assume that user defined typevar never starts with '#'
    return $ TVar $ "#" ++ show n

unify :: MonoType -> MonoType -> Infer ()
unify (TLit a) (TLit b)
    | a == b = return ()
unify (TCon a) (TCon b)
    | a == b = return ()
    -- type alias in the futrue, maybe not an error
    | otherwise = throwError $ CantUnify (TCon a) (TCon b)
unify (TFun xs x) (TFun ys y)
    | L.length xs == L.length ys = do
        mapM_ (uncurry unify) (L.zip xs ys)
        unify x y
unify (TApp x xs) (TApp y ys)
    | L.length xs == L.length ys = do
        unify x y
        mapM_ (uncurry unify) (L.zip xs ys)
unify (TVar x) (TVar y)
    | x == y = return ()
unify (TVar x) ty = do
    a <- gets assign
    case M.lookup x a of
        Just ty' -> unify ty ty'
        Nothing -> modify $ \s -> s{ assign = M.insert x ty a }
unify ty (TVar x) =
    unify (TVar x) ty
unify ty1 ty2 = throwError $ CantUnify ty1 ty2

mergeType :: MonoType -> Infer MonoType
mergeType t@(TLit _) = return t
mergeType t@(TCon _) = return t
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

subst :: M.Map Name MonoType -> MonoType -> MonoType
subst s t@(TLit _) = t
subst s t@(TCon _) = t
subst s t@(TVar x) =
    -- may be captrued by gamma?
    fromMaybe t (M.lookup x s)
subst s (TFun xs x) =
    TFun (fmap (subst s) xs) (subst s x)
subst s (TApp x xs) =
    TApp x (fmap (subst s) xs)

freevar :: MonoType -> Infer (S.Set Name)
freevar (TLit _) = return S.empty
freevar (TCon _) = return S.empty
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

instantiate :: PolyType -> Infer MonoType
instantiate (PolyType xs ty) = do
    vars <- replicateM (L.length xs) newvar
    let s = M.fromList (L.zip xs vars)
    return $ subst s ty

generalize :: MonoType -> Infer PolyType
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

binopType :: LitType -> LitType -> LitType -> MonoType
binopType ty1 ty2 ty3 = TFun [TLit ty1, TLit ty2] (TLit ty3)

typeInferPrim :: Prim -> MonoType
typeInferPrim IAdd =
    binopType LitInt LitInt LitInt
typeInferPrim ISub =
    binopType LitInt LitInt LitInt
-- todo: more primitives
typeInferPrim (ICmp _) =
    binopType LitInt LitInt LitBool
typeInferPrim _ = undefined

typeInfer :: Expr (Maybe Type) -> Infer (MonoType, Expr Type)
typeInfer (ELit lit) =
    let ty = typeInferLiteral lit in
    return (TLit ty, ELit lit)
typeInfer (EVar x) = do
    pty <-  asks $ M.lookup x
    case pty of
        Just pty -> do
            mty <- instantiate pty
            return (mty, EVar x)
        Nothing -> throwError $ VarNotInScope x
typeInfer (EFun xs x) = do
    let (args, typs) = unzip xs
    mtys <- forM typs $ \case
        Just (Mono ty) -> return ty
        Just (Poly ty) -> undefined -- unreachable in rank1 type system
        Nothing -> newvar
    let ptys = fmap (PolyType []) mtys
    let s = M.fromList $ L.zip args ptys
    (xty, x') <- local (M.union s) (typeInfer x)
    let xs' = L.zip args (fmap Mono mtys)
    return (TFun mtys xty, EFun xs' x')
typeInfer (EApp func args) = do
    (fty, func') <- typeInfer func
    lst <- mapM typeInfer args
    let (aty, args') = unzip lst
    res <- newvar
    unify fty (TFun aty res)
    return (res, EApp func' args')
typeInfer (ELet x (Just (Mono ty)) e1 e2) = do
    (ty1, e1') <- typeCheck e1 ty
    pty <- generalize ty1
    local (M.insert x pty) (typeInfer e2)
typeInfer (ELet x (Just (Poly ty)) e1 e2) =
    undefined
typeInfer (ELet x Nothing e1 e2) = do
    ty <- newvar
    (ty1, e1') <- typeInfer e1
    pty <- generalize ty1
    local (M.insert x pty) (typeInfer e2)
typeInfer (EIfte cond trbr flbr) = do
    (_, cond') <- typeCheck cond (TLit LitBool)
    (ty, trbr') <- typeInfer trbr
    (_, flbr') <- typeCheck flbr ty
    return (ty, EIfte cond' trbr' flbr')
typeInfer (EOpr op args) = do
    let op' = typeInferPrim op
    lst <- mapM typeInfer args
    let (tys, args') = unzip lst
    res <- newvar
    unify op' (TFun tys res)
    return (res, EOpr op args')
typeInfer (EFix defs body) = do
    ctx <- M.fromList <$> forM defs (\def -> do
        v <- newvar
        return (func def , PolyType [] v))
    lst <-  local (M.union ctx) $ mapM typeInferDef defs
    let (tys, defs') = unzip lst
    tys' <- mapM generalize tys
    let lst = zip tys' defs'
    let ctx = M.fromList $ zip (fmap func defs') tys'
    (ty',body') <- local (M.union ctx) (typeInfer body)
    let defs' = fmap snd lst
    return (ty', EFix defs' body')
typeInfer (EAnno expr (Just (Poly pty))) =
    undefined
typeInfer (EAnno expr (Just (Mono mty))) =
    typeCheck expr mty
typeInfer (EAnno expr Nothing) =
    typeInfer expr

typeInferDef :: Def (Maybe Type) -> Infer (MonoType, Def Type)
typeInferDef Def{ func, args, body, typ } = do
    let expr = EFun args body
    (ty', args') <- typeInfer expr
    case typ of
        Just (Mono ty) -> unify ty ty'
        Just (Poly ty) -> undefined
        Nothing -> return ()
    case args' of
        (EFun xs' body') -> return (ty', Def { func, args = xs', body = body', typ = Mono ty'})
        _ -> undefined


typeCheck :: Expr (Maybe Type) -> MonoType -> Infer (MonoType, Expr Type)
{-
typeCheck (EFun xs e) (TFun ys y)
    | L.length xs == L.length ys = do
        mapM_ (uncurry unify) (L.zip xs ys)
        unify x y
    | otherwise = throwError InferError
-}


typeCheck expr ty = do
    (ty', expr') <- typeInfer expr
    unify ty' ty
    return (ty', expr')


typeInferTop :: Expr (Maybe Type)  -> Either [InferError] (Expr Type)
typeInferTop expr =
    case evalRWST (typeInfer expr) M.empty emptyState of
        Left err -> Left [err]
        Right ((ty,expr'),err) -> Right expr'




{-
insertEnv :: Name -> PolyType Name -> Infer ()
insertEnv x pty = do
    ctx <- gets context
    modify $ \s -> s{context = M.insert x (Just pty) ctx}





-- typeInfer ctx (ELam xs e) = M.lookup x ctx
typeInfer expr = undefined


typeCheck :: Expr Name -> MonoType Name -> Infer 
-}