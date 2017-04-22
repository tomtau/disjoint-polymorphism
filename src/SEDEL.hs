{-# LANGUAGE OverloadedStrings, ViewPatterns #-}


module SEDEL
  ( evalFile
  ) where

import           Control.Exception (SomeException, try)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)

import           SEDEL.Environment
import           SEDEL.Parser.Parser2 (parseExpr)
import           SEDEL.PrettyPrint
import           SEDEL.Source.Typing
import qualified SEDEL.Target.CallByNeed as C

type Result = Either Doc String

ret :: Doc -> Result
ret = Left

parseExpectedOutput :: Text -> Maybe Text
parseExpectedOutput source =
  let firstLine = T.takeWhile (/= '\n') source
  in fmap T.strip (T.stripPrefix "-->" (T.strip firstLine))

readTry :: IO String -> IO (Either SomeException String)
readTry = try

eval :: String -> IO Result
eval inp =
  case parseExpr inp of
    Left err -> return $ ret $ warn "Syntax error" <+> text err
    Right abt ->
      let res = runTcMonad emptyCtx (tcModule abt)
      in case res of
           Left err -> return $ ret err
           Right (_, tar) -> do
             v <- C.evaluate tar
             return (Right . show $ v)

evalFile :: FilePath -> IO ((Doc, Maybe Doc), Bool)
evalFile path = do
  msg <- readTry $ readFile path
  let failed d = return ((d, Nothing), False)
      failWith d d' = return ((d, Just d'), False)
      succed d = return ((d, Nothing), True)
  case msg of
    Left err -> failed $ warn "Load file error" <+> text (show err)
    Right contents -> do
      value <- eval contents
      case value of
        Left err -> failed err
        Right tm ->
          case parseExpectedOutput (T.pack contents) of
            Nothing -> failed $ warn "No expectation" <+> text tm
            Just (T.unpack -> expinp) ->
              if tm == expinp
                then succed (text tm)
                else failWith (text tm) (text expinp)
