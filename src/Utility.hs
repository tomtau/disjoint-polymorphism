{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}


module Utility
  ( evalFile
  ) where

import           Protolude
import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)

import           Environment
import           PrettyPrint
import           Source.Parser (parseExpr)
import           Source.Typing
import qualified Target.CBN as C

type Result = Either Doc Text

ret :: Doc -> Result
ret d = Left d

parseExpectedOutput :: Text -> Maybe Text
parseExpectedOutput source =
  let firstLine = T.takeWhile (/= '\n') source
  in fmap T.strip (T.stripPrefix "-->" (T.strip firstLine))

readTry :: IO Text -> IO (Either SomeException Text)
readTry = try

eval :: Text -> Result
eval inp =
  case parseExpr (toS inp) of
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
               Nothing -> failed $ warn "No expectation" <+> text (toS tm)
               Just expinp ->
                 if tm == expinp
                   then succed (text (toS tm))
                   else failWith (text (toS tm)) (text (toS expinp))
