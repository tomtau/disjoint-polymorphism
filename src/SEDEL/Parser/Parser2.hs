module SEDEL.Parser.Parser2 where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

import SEDEL.Source.Syntax
import SEDEL.Common

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

semi :: Parser String
semi = symbol ";"

top :: Parser ()
top = symbol "(" *> symbol ")" *> return ()

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws =
  [ "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "def"
  , "type"
  , "defrec"
  , "forall"
  , "trait"
  , "new"
  , "Trait"
  , "main"
  , "inherits"
  , "undefined"
  , "Double"
  , "String"
  , "Bool"
  , "true"
  , "false"
  ]

lidentifier :: Parser String
lidentifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> lowerChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an lidentifier"
        else return x

uidentifier :: Parser String
uidentifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> upperChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an lidentifier"
        else return x

expr, term, factor :: Parser Expr

expr = makeExprParser term pOperators

term = funapp

funapp :: Parser Expr
funapp = foldl1 App <$> some factor

factor =
  choice
    [ pLambda
    , pBLambda
    , pLet
    , pIf
    , parens pAnno
    , (LitV . fromInteger) <$> integer
    , StrV <$> stringLiteral
    , evar <$> lidentifier
    , bconst
    , parens expr
    ]

bconst :: Parser Expr
bconst =
  choice
    [ rword "true" *> pure (BoolV True)
    , rword "false" *> pure (BoolV False)
    , rword "undefined" *> pure Bot
    , top *> pure Top
    ]

pLambda :: Parser Expr
pLambda = do
  symbol "\\"
  xs <- some lidentifier
  symbol "->"
  e <- expr
  return $ foldr elam (elam (last xs) e) (init xs)

pBLambda :: Parser Expr
pBLambda = do
  symbol "/\\"
  xs <- some ctyparam
  symbol "."
  e <- expr
  return $ foldr dlam (dlam (last xs) e) (init xs)

ctyparam :: Parser (String, Type)
ctyparam =
  choice
    [ do n <- uidentifier
         return (n, TopT)
    , parens
        (do n <- uidentifier
            symbol "*"
            t <- pType
            return (n, t))
    ]

pLet :: Parser Expr
pLet = do
  rword "let"
  n <- lidentifier
  symbol ":"
  t <- pType
  symbol "="
  e1 <- expr
  rword "in"
  e2 <- expr
  return $ elet n t e1 e2

pIf :: Parser Expr
pIf = do
  rword "if"
  a <- expr
  rword "then"
  b <- expr
  rword "else"
  c <- expr
  return $ If a b c

pAnno :: Parser Expr
pAnno = do
  e <- expr
  symbol ":"
  t <- pType
  return $ Anno e t

pAcc :: Parser Expr
pAcc = do
  e <- expr
  symbol "."
  n <- lidentifier
  return $ Acc e n


pOperators :: [[Operator Parser Expr]]
pOperators =
  [ [ InfixL (PrimOp (Arith Mul) <$ symbol "*")
    , InfixL (PrimOp (Arith Div) <$ symbol "/")
    ]
  , [ InfixL (PrimOp Append <$ symbol "++")
    , InfixL (PrimOp (Arith Add) <$ symbol "+")
    , InfixL (PrimOp (Arith Sub) <$ symbol "-")
    ]
  , [ InfixN (PrimOp (Comp Lt) <$ symbol "<")
    , InfixN (PrimOp (Comp Gt) <$ symbol ">")
    ]
  , [ InfixN (PrimOp (Comp Equ) <$ symbol "==")
    , InfixN (PrimOp (Comp Neq) <$ symbol "/=")
    ]
  , [InfixL (PrimOp (Logical LAnd) <$ symbol "&&")]
  , [InfixL (PrimOp (Logical LOr) <$ symbol "||")]
  , [InfixL (Merge <$ symbol ",,")]
  ]

pType :: Parser Type
pType = rword "Double" *> pure NumT
