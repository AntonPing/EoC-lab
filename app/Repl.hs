module Repl where

import qualified System.Console.Haskeline as HL
import System.Console.Repline
import System.Environment
import System.Exit
import Control.Applicative
import Control.Monad.State

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

import Syntax

import Printer
import Parser
import Interp
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import TypeCheck (typeInferTop)

type IState = M.Map String (Expr Name)
type Repl = HaskelineT (StateT IState (ExceptT ReplError IO))

data ReplError = ReplError

-- Options --
help :: Cmd Repl
help arg = liftIO $ print $ "Help: " ++ show arg

quit :: Cmd Repl
quit arg = abort

load :: Cmd Repl
load arg = do
    str <- liftIO $ readFile arg
    eval str

eval :: Cmd Repl
eval arg = do 
    expr <- replParse arg
    liftIO $ print expr
    ty <- replTypeInfer expr
    liftIO $ print ty
    val <- replInterp (eraseType expr)
    liftIO $ print val
    return ()

replParse :: String -> Repl (Expr (Maybe Type))
replParse arg = 
    case parseExpr arg of
        Left err -> do
            liftIO $ putStrLn "parser failed."
            lift $ throwError ReplError
        Right expr -> return expr
    

replTypeInfer :: Expr (Maybe Type) -> Repl (Expr Type)
replTypeInfer expr =
    case typeInferTop expr of
        Left errs -> do
            liftIO $ putStrLn "typecheck failed."
            liftIO $ prettyPrint errs
            lift $ throwError ReplError
        Right ty -> return ty

replInterp :: Expr () -> Repl Value
replInterp expr = do
    res <- liftIO $ interp expr
    case res of
        Left err -> do
            liftIO $ putStrLn "runtime errror."
            liftIO $ print err
            lift $ throwError ReplError
        Right val -> return val


-- Settings --
replBan :: MultiLine -> Repl String
replBan SingleLine = return "> "
replBan MultiLine = return "  "

replCmd :: Cmd Repl
replCmd = eval

replOpts :: Options Repl
replOpts =
    [ ("help", help)
    , ("eval", eval)
    , ("quit", quit)
    , ("load", load)
    ]

completer :: Monad m => WordCompleter m
completer n = do
  let names = fmap ((':' :) . fst) replOpts
  return $ L.filter (L.isPrefixOf n) names

replInit :: Repl ()
replInit = liftIO $ putStrLn "Welcome!"

replFinal :: Repl ExitDecision
replFinal = liftIO $ do
    putStrLn "Bye!"
    return Exit

setting :: ReplOpts (StateT IState (ExceptT ReplError IO))
setting = ReplOpts
  { banner           = replBan
  , command          = replCmd
  , options          = replOpts
  , prefix           = Just ':'
  , multilineCommand = Just "multi"
  , tabComplete      = Word0 completer
  , initialiser      = replInit
  , finaliser        = replFinal
  }

repl :: IO ()
repl = do
    res <- (runExceptT . flip evalStateT M.empty . evalReplOpts) setting
    case res of
        Left err -> putStrLn "repl failed."
        Right _ -> return ()