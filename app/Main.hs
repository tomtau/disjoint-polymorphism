module Main where

import           PrettyPrint              (pprint)
import           Source.Parser            (parseExpr)

import System.Exit
import System.Environment (getArgs)
import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import System.Console.Repline
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Source.Typing (infer)
import Target.Dynamics (evaluate)
import qualified Target.CBN as CBN
import Env (runTcMonad)

-- Types

type Repl a = HaskelineT IO a

hoistErr :: Either T.Text a -> Repl a
hoistErr (Left str) = do
  liftIO $ TI.putStrLn str
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

  res <- hoistErr . runTcMonad $ CBN.evaluate tar

  liftIO . putStrLn $ "Evaluation result"
  liftIO . print $ res


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
