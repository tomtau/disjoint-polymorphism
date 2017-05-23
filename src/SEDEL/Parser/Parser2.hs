module SEDEL.Parser.Parser2 where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

import SEDEL.Source.Syntax
import SEDEL.Common



------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: Parser Expr
expr = makeExprParser term pOperators

term :: Parser Expr
term = postfixChain factor (fapp <|> bapp)

fapp :: Parser (Expr -> Expr)
fapp = do
  e <- factor
  return (`App` e)

bapp :: Parser (Expr -> Expr)
bapp = do
  e <- pType
  return (`TApp` e)


factor :: Parser Expr
factor = postfixChain atom (rmOperator <|> dotOperator)

dotOperator :: Parser (Expr -> Expr)
dotOperator = do
  symbol "."
  k <- lidentifier
  return (`Acc` k)

rmOperator :: Parser (Expr -> Expr)
rmOperator = do
  symbol "\\"
  (l, t) <-
    braces
      (do l <- lidentifier
          symbol ":"
          t <- pType
          return (l, t))
  return (\e -> Remove e l t)

atom :: Parser Expr
atom =
  choice
    [ pLambda
    , pBLambda
    , pLet
    , pIf
    , (LitV . fromInteger) <$> integer
    , StrV <$> stringLiteral
    , evar <$> lidentifier
    , record
    , bconst
    , expAnnoOrParenOrTop
    ]

record :: Parser Expr
record = braces (mkRecds <$> sepBy1 recordField (symbol ","))

recordField :: Parser (Label, Expr)
recordField = do
  l <- lidentifier
  symbol "="
  e <- expr
  return (l,e)

bconst :: Parser Expr
bconst =
  choice
    [ rword "true" *> pure (BoolV True)
    , rword "false" *> pure (BoolV False)
    , rword "undefined" *> pure Bot
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


data InParens = Colon Expr Type | Nope Expr


-- (x : A) or (e)
expAnnoOrParenOrTop :: Parser Expr
expAnnoOrParenOrTop =
  let tryParens =
        parens $
        choice
          [ do e1 <- try (expr >>= (\e1 -> symbol ":" >> return e1))
               e2 <- pType
               return $ Colon e1 e2
          , Nope <$> expr
          ]
  in do bd <- tryParens
        case bd of
          Colon a b -> return $ Anno a b
          Nope e -> return e

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


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

pType :: Parser Type
pType = makeExprParser atype tOperators

tOperators :: [[Operator Parser Type]]
tOperators =
  [ [ Postfix
        (bracket $
         sepBy1 pType (symbol ",") >>= \xs ->
           return $ \f -> foldl OpApp (OpApp f (head xs)) (tail xs))
    ]
  , [InfixL (And <$ symbol "&&")]
  , [InfixR (Arr <$ symbol "->")]
  ]

atype :: Parser Type
atype =
  choice
    [pForall, pTrait, tvar <$> uidentifier, recordType, tconst, parens pType]

pForall :: Parser Type
pForall = do
  symbol "\\/"
  xs <- some ctyparam
  symbol "."
  t <- pType
  return $ foldr tforall (tforall (last xs) t) (init xs)

pTrait :: Parser Type
pTrait = do
  rword "Trait"
  ts <- bracket $ sepBy1 pType (symbol ",")
  if length ts == 1
    then return $ Arr TopT (head ts)
    else return $ foldl1 Arr ts

recordType :: Parser Type
recordType = braces (mkRecdsT <$> sepBy1 recordFieldType (symbol ","))

recordFieldType :: Parser (Label, Type)
recordFieldType = do
  l <- lidentifier
  symbol ":"
  e <- pType
  return (l, e)


tconst :: Parser Type
tconst =
  choice
    [ rword "Double" *> pure NumT
    , rword "String" *> pure StringT
    , rword "Bool" *> pure BoolT
    , rword "T" *> pure TopT
    ]

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

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

bracket :: Parser a -> Parser a
bracket = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexeme L.integer

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

postfixChain :: Parser a -> Parser (a -> a) -> Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x =
      (do f <- op
          rest $ f x) <|>
      return x

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
  , "T"
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
