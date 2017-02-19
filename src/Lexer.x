{

module Lexer (
  Token(..),
  TokenClass(..),
  lexer,
  showPosn,
  Alex(..),
  alexError,
  runAlex,
  ) where

}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$white = [\ \t\n\r]
$paren = [\(\)\[\]\{\}]

@keyword = let | in
         | int | bool
         | if  | then | else
         | def | type | module
         | defrec

@compop = "==" | "/=" | \/\\ | \\\/ | "->" | ",,"

@op = \\ | "/" | ":" | "," | "."
    | "+" | "-" | "*" | "T" | "@"
    | "&" | "=" | ">" | "<" | ";" | "[" | "]"

@id = $alpha [$alpha $digit \_ \']*

tokens :-

<0> $white+             { skip }
<0> "--".*              { skip }
<0> $paren              { mkT (const TSym) }

<0> true                { mkT (const (TBool True))}
<0> false               { mkT (const (TBool False))}
<0> @keyword            { mkT (const TKey) }
<0> @compop             { mkT (const TSym) }
<0> @op                 { mkT (const TSym) }
<0> $digit+             { mkT (TInt . read) }
<0> @id                 { mkT TId }


{

data Token = T AlexPosn TokenClass String deriving (Show, Eq)

data TokenClass = TId String
                | TInt Int
                | TBool Bool
                | TSym
                | TKey
                | TEOF
                deriving (Show, Eq)

mkT :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mkT c (p,_,_,str) len = return (T p (c inp) inp)
  where inp = take len str


lexError :: String -> Alex a
lexError s = do
  (p,c,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
                (if (not (null input))
                   then " before " ++ show (head input)
                   else " at end of file"))

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':' : show col

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ T p TEOF ""

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
