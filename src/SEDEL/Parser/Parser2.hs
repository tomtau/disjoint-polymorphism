module SEDEL.Parser.Parser2 (parseExpr) where

import           Control.Arrow (first)
import           Control.Monad (void)
import           Data.Maybe (fromMaybe)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String
import           Unbound.LocallyNameless

import           SEDEL.Common
import           SEDEL.Source.Syntax

parseExpr :: String -> Either String Module
parseExpr s =
  case runParser (sc *> prog) "" s of
    Left err -> Left $ parseErrorPretty err
    Right e -> Right e

------------------------------------------------------------------------
-- Programs
------------------------------------------------------------------------

prog :: Parser Module
prog =
   -- hack for repl
  try (expr >>= \e -> return $ Module [] (DefDecl (TmBind "main" [] [] e Nothing))) <|> prog'

prog' :: Parser Module
prog' = do
  decls <- sepEndBy sedel (symbol ";")
  m <- optional mainDecl
  let decl = fromMaybe (DefDecl (TmBind "main" [] [] Top Nothing)) m
  return $ Module decls decl

mainDecl :: Parser SDecl
mainDecl = do
  rword "main"
  symbol "="
  e <- expr
  return $ DefDecl (TmBind "main" [] [] e Nothing)

sedel :: Parser SDecl
sedel = DefDecl <$> tmBind <|> TypeDecl <$> tyBind

tmBind :: Parser TmBind
tmBind = do
  n <- lidentifier
  ts <- many ctyparam
  xs <- many param
  ret <- optional (symbol ":" *> pType)
  symbol "="
  e <- expr
  return $ TmBind n (map (first s2n) ts) (map (first s2n) xs) e ret


tyBind :: Parser TypeBind
tyBind = do
  rword "type"
  n <- uidentifier
  ts <- optional typaramList
  symbol "="
  t <- pType
  return $ TypeBind n (fromMaybe [] ts) t

typaramList :: Parser [(TyName, Kind)]
typaramList =
  brackets $
  sepBy1 (uidentifier >>= \n -> return (s2n n, Star)) (symbol ",") <|> return []

param :: Parser (String, Maybe Type)
param =
  choice
    [ lidentifier >>= \n -> return (n, Nothing)
    , parens $ do
        n <- lidentifier <|> symbol "_"
        symbol ":"
        t <- pType
        return (n, Just t)
    ]

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: Parser Expr
expr = makeExprParser term pOperators

term :: Parser Expr
term = postfixChain factor (try fapp <|> bapp)

fapp :: Parser (Expr -> Expr)
fapp = do
  e <- factor
  return (`App` e)

bapp :: Parser (Expr -> Expr)
bapp = do
  e <- pType
  return (`TApp` e)


factor :: Parser Expr
factor = postfixChain atom (rmOperator <|> dotOperator <|> colonOperator)

dotOperator :: Parser (Expr -> Expr)
dotOperator = do
  symbol "."
  k <- lidentifier
  return (`Acc` k)

colonOperator :: Parser (Expr -> Expr)
colonOperator = do
  symbol ":"
  t <- pType
  return (`Anno` t)


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
    , parens expr
    ]

record :: Parser Expr
record = braces (mkRecds' <$> sepBy1 tmBind (symbol ","))

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
  xs <- some pCtyparam
  symbol "."
  e <- expr
  return $ foldr dlam (dlam (last xs) e) (init xs)


pTrait :: Parser Expr
pTrait = do
  rword "trait"
  ts <- many ctyparam
  xs <- parens $ sepBy recordFieldType (symbol ",")
  i <- optional inherits
  ret <- optional (symbol ":" *> pType)
  undefined

inherits :: Parser [Expr]
inherits = undefined

constrainTy :: Parser (String, Type)
constrainTy = do
  n <- uidentifier
  symbol "*"
  t <- pType
  return (n, t)

pCtyparam :: Parser (String, Type)
pCtyparam =
  choice
    [ do n <- uidentifier
         return (n, TopT)
    , parens constrainTy
    ]

ctyparam :: Parser (String, Type)
ctyparam =
  choice
    [ do n <- uidentifier
         return (n, TopT)
    , brackets constrainTy
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
        (brackets $
         sepBy1 pType (symbol ",") >>= \xs ->
           return $ \f -> foldl OpApp (OpApp f (head xs)) (tail xs))
    ]
  , [InfixL (And <$ symbol "&")]
  , [InfixR (Arr <$ symbol "->")]
  ]

atype :: Parser Type
atype =
  choice
    [pForall, traitType, tvar <$> uidentifier, recordType, tconst, parens pType]

pForall :: Parser Type
pForall = do
  rword "forall"
  xs <- some pCtyparam
  symbol "."
  t <- pType
  return $ foldr tforall (tforall (last xs) t) (init xs)

traitType :: Parser Type
traitType = do
  rword "Trait"
  ts <- brackets $ sepBy1 pType (symbol ",")
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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

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
