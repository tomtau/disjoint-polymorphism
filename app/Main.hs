module Main where

import           Control.Exception (SomeException, try)
import           Control.Monad.State.Strict
import           Data.List (isPrefixOf)
import           Environment
import           PrettyPrint
import           Source.Parser (parseExpr)
import           Source.Typing
import           System.Console.Repline
import           System.Environment (getArgs)
import           System.Exit
import qualified Target.Dynamics as T
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)


data ReplState = ReplState {
  replCtx   :: Ctx
  }

initState :: ReplState
initState = ReplState {
  replCtx = emptyCtx
  }

type Repl a = HaskelineT (StateT ReplState IO) a


getCtx :: Repl ReplState
getCtx = lift get

putCtx :: ReplState -> Repl ()
putCtx = lift . put

readTry :: IO String -> Repl (Either SomeException String)
readTry = liftIO . try

putMsg :: String -> Repl ()
putMsg = liftIO . putStrLn

ppMsg :: Doc -> Repl ()
ppMsg d = liftIO . putDoc $ d <> line

-- Execution
exec :: String -> Repl ()
exec source =
  case parseExpr source of
    Left err -> ppMsg $ warn "Syntax error" <+> text err
    Right abt -> do
      env <- getCtx
      let res = runTcMonad (replCtx env) (tcModule abt)
      case res of
        Right (typ, tar) -> do
          putMsg "Typing result"
          ppMsg $ colon <+> blue (pprint typ)
          let r = T.evaluate tar
          putMsg "\nEvaluation result"
          ppMsg $ text "=>" <+> blue (text (show r))
        Left err -> ppMsg err


-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- readTry $ readFile (unwords args)
  case contents of
    Left err -> ppMsg $ warn "Load file error" <+> text (show err)
    Right s -> do
      resetCtx
      exec s

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess


resetCtx :: Repl ()
resetCtx = do
  st <- getCtx
  putCtx ReplState {
    replCtx = emptyCtx
    }

reset :: [String] -> Repl ()
reset _ = do
  resetCtx
  putMsg "Context cleaned"

debug :: [String] -> Repl ()
debug _ = do
  ReplState ctx <- getCtx
  putMsg "Not implemented!"


-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load"  , fileCompleter)]

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":quit", ":reset", ":debug"]
  return $ filter (isPrefixOf n) cmds


options :: [(String, [String] -> Repl ())]
options = [("load", load), ("reset", reset), ("quit", quit), ("debug", debug)]

shell :: Repl a -> IO ()
shell pre =
  flip evalStateT initState $
  evalRepl "> " exec options (Prefix (wordCompleter comp) defaultMatcher) pre


ini :: Repl ()
ini = putMsg "Welcome!"

-- Top level

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell ini
    [fname] -> shell (load [fname])
    _ -> putStrLn "invalid arguments"
