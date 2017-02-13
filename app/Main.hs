module Main where

import qualified Data.Text
import           PrettyPrint              (pprint)
import           Source.Parser            (parseExpr)

import System.Exit
import System.Environment (getArgs)
import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import System.Console.Repline

import Source.Typing (infer)
import Target.Dynamics (evaluate)
import Env (runTcMonad)
import PrettyPrint (pprint)

-- Types

type Repl a = HaskelineT IO a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Left str) = do
  liftIO $ putStrLn (show str)
  abort
hoistErr (Right val) = return val


-- Execution
exec :: String -> Repl ()
exec source = do

  abt <- hoistErr $ parseExpr source

  (typ, tar) <- hoistErr . runTcMonad $ infer abt

  liftIO . putStrLn $ "Source typing result"
  liftIO . putStrLn . pprint $ typ

  liftIO . putStrLn $ ""


  liftIO . putStrLn $ "Target expression"
  liftIO . putStrLn . pprint $ tar

  liftIO . putStrLn $ ""

  let res = evaluate tar

  liftIO . putStrLn $ "Evaluation result"
  liftIO . putStrLn . pprint $ res


-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ readFile (unwords args)
  exec contents

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess


-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load"  , fileCompleter)]

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":load", ":quit"]
  return $ filter (isPrefixOf n) cmds


options :: [(String, [String] -> Repl ())]
options = [("load", load) , ("quit", quit)]

-- Entry point

completer :: CompleterStyle IO
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre =  evalRepl "> " exec options completer pre


-- Top level

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (return ())
    [fname] -> shell (load [fname])
    _ -> putStrLn "invalid arguments"
