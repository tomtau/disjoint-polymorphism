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

import Data.Char (chr, isHexDigit, isOctDigit)
import Numeric (readOct)
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$vchar = [$alpha $digit \_ \']
$white = [\ \t\n\r]
$paren = [\(\)\[\]\{\}]

@keyword = let | in
         | Double | Bool | String
         | if  | then | else
         | def | type | module
         | defrec | forall | trait
         | new | Trait | main
         | inherits | val

@compop = "==" | "/=" | \/\\ | "->" | ",," | "()" | "=>" | "++"

@op = \\ | "/" | ":" | "," | "."
    | "+" | "-" | "*" | "T"
    | "&" | "=" | ">" | "<" | ";"
    | "[" | "]" | "_"

$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
@octEscape  = [0123]? $octdig{1,2}
@hexEscape  = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])
@gap     = \\ $white+ \\
@string  = . # [\"\\] | " " | @charEscape | @gap
@id = $alpha [$alpha $digit \_ \']*

tokens :-

<0> $white+                                    { skip }
<0> "--".*                                     { skip }
<0> "{-"                                       { nestedComment }
<0> $paren                                     { mkT (const TSym) }

<0> true                                       { mkT (const (TBool True))}
<0> false                                      { mkT (const (TBool False))}
<0> @keyword                                   { mkT (const TKey) }
<0> @compop                                    { mkT (const TSym) }
<0> @op                                        { mkT (const TSym) }
<0> \-?[$digit]*\.?[$digit]+                   { mkT (TNum . read) }
<0> [A-Z] [$vchar]*                            { mkT Tupperid }
<0> \_ [$alpha \_] [$vchar]* | [a-z] [$vchar]* { mkT Tlowerid }
<0> \" @string* \"                             { mkT (TStr . convChar . tail . init) }


{

data Token = T AlexPosn TokenClass String deriving (Show, Eq)

data TokenClass = Tlowerid String
                | Tupperid String
                | TNum Double
                | TBool Bool
                | TStr String
                | TSym
                | TKey
                | TEOF
                deriving (Show, Eq)

mkT :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mkT c (p,_,_,str) len = return (T p (c inp) inp)
  where inp = take len str


lexicalError :: String -> a
lexicalError = error . ("lexical unescape error: " ++)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> String
convChar ('\\':'u':s@(d1:d2:d3:d4:s')) =
  -- TODO: this is the wrong place for handling unicode escapes
  -- according to the Java Language Specification. Unicode escapes can
  -- appear anywhere in the source text, and are best processed
  -- before lexing.
  if all isHexDigit [d1,d2,d3,d4]
  then toEnum (read ['0','x',d1,d2,d3,d4]):convChar s'
  else lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':'u':s) =
  lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':c:s) =
  if isOctDigit c
  then convOctal maxRemainingOctals
  else (case c of
          'b' -> '\b'
          'f' -> '\f'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          '\'' -> '\''
          '\\' -> '\\'
          '"' -> '"'
          _ -> badEscape):convChar s
  where maxRemainingOctals =
          if c <= '3' then 2 else 1
        convOctal n =
          let octals = takeWhile isOctDigit $ take n s
              noctals = length octals
              toChar = toEnum . fst . head . readOct
          in toChar (c:octals):convChar (drop noctals s)
        badEscape = lexicalError $ "bad escape \"\\" ++ c:"\""
convChar ("\\") =
  lexicalError "bad escape \"\\\""
convChar (x:s) = x:convChar s
convChar "" = ""

lexError :: String -> Alex a
lexError s = do
  (p,c,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
                (if (not (null input))
                   then " before " ++ show (head input)
                   else " at end of file"))


-- Nested comment block
nestedComment :: AlexInput -> Int -> Alex Token
nestedComment _ _ = do
  input <- alexGetInput
  go 1 input
  where
    go 0 input = do
      alexSetInput input
      alexMonadScan
    go n input = do
      case alexGetByte input of
        Nothing -> err input
        Just (c, input) -> do
          case chr (fromIntegral c) of
            '-' -> do
              case alexGetByte input of
                Nothing -> err input
                Just (125, input) -> go (n - 1) input
                Just (_, input) -> go n input
            '\123' -> do
              case alexGetByte input of
                Nothing -> err input
                Just (c', input)
                  | c' == fromIntegral (ord '-') -> go (n + 1) input
                Just (_, input) -> go n input
            _ -> go n input
    err input = do
      alexSetInput input
      lexError "error in nested comment"



showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':' : show col

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ T p TEOF ""

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
