module Main where

import           Control.Exception (SomeException, try)
import           Control.Monad.State.Strict
import           Data.List (isPrefixOf)
import           Environment
import           PrettyPrint (pprint)
import           Source.Parser (parseExpr)
import           Source.Syntax (topType)
import           Source.Typing
import           System.Console.Repline
import           System.Environment (getArgs)
import           System.Exit
import qualified Target.CBN as CBN


data ReplState = ReplState {
  replCtx   :: Env
  }

initState :: ReplState
initState = ReplState {
  replCtx = emptyEnv
  }

type Repl a = HaskelineT (StateT ReplState IO) a


getCtx :: Repl ReplState
getCtx = lift get

putCtx :: ReplState -> Repl ()
putCtx = lift . put


hoistErr :: Either String a -> Repl a
hoistErr (Left str) = do
  liftIO $ putStrLn str
  abort
hoistErr (Right val) = return val

readTry :: IO String -> Repl (Either SomeException String)
readTry = liftIO . try

putMsg :: String -> Repl ()
putMsg s = liftIO . putStrLn $ (s ++ "\n")

-- Execution
exec :: String -> Repl ()
exec source = do
  abt <- hoistErr $ parseExpr source
  env <- getCtx
  (typ, tar, tEnv) <- hoistErr . (runTcMonad (replCtx env)) $ tcModule abt
  if topType typ
    then putMsg "Declaration added!"
    else do
      putMsg "Source typing result"
      putMsg $ pprint typ
      let res = CBN.evaluate tEnv tar
      putMsg "Evaluation result"
      putMsg (show res)


-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- readTry $ readFile (unwords args)
  case contents of
    Left err -> putMsg . show $ err
    Right s -> exec s

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess


resetCtx :: Repl ()
resetCtx = do
  st <- getCtx
  putCtx ReplState {
    replCtx = emptyEnv
    }

reset :: [String] -> Repl ()
reset _ = do
  resetCtx
  putMsg "Context cleaned"

debug :: [String] -> Repl ()
debug _ = do
  ReplState ctx <- getCtx
  putMsg . show $ ctx


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
