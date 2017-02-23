module Utility
  ( evalFile
  ) where

import           Control.Exception (SomeException, try)
import           Data.Char (isSpace)
import           Environment
import           PrettyPrint
import           Source.Parser (parseExpr)
import           Source.Typing
import qualified Target.CBN as C
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)

type Result = Either Doc String

ret :: Doc -> Result
ret d = Left d


parseExpectedOutput :: String -> Maybe String
parseExpectedOutput source =
  let firstLine = takeWhile (/= '\n') source
  in case firstLine of
       '-':'-':'>':r -> Just $ strip r
       _ -> Nothing

strip :: String -> String
strip = f . f
  where
    f = reverse . dropWhile isSpace

readTry :: IO String -> IO (Either SomeException String)
readTry = try

eval :: String -> Result
eval inp =
  case parseExpr inp of
    Left err -> ret $ warn "Syntax error" <+> text err
    Right abt ->
      let res = runTcMonad emptyCtx (tcModule abt)
      in case res of
           Left err -> ret err
           Right (_, tar, tEnv) -> return . show . C.evaluate tEnv $ tar

evalFile :: FilePath -> IO ((Doc, Maybe Doc), Bool)
evalFile path = do
  msg <- readTry $ readFile path
  let failed d = return ((d, Nothing), False)
      failWith d d' = return ((d, Just d'), False)
      succed d = return ((d, Nothing), True)
  case msg of
    Left err -> failed $ warn "Load file error" <+> text (show err)
    Right contents ->
      let value = eval contents
      in case value of
           Left err -> failed err
           Right tm ->
             case parseExpectedOutput contents of
               Nothing -> failed $ warn "No expectation" <+> text tm
               Just expinp ->
                 if tm == expinp
                   then succed (text tm)
                   else failWith (text tm) (text expinp)
