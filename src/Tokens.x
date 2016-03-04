{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  let                           { \s -> TLet }
  in                            { \s -> TIn }
  int                           { \s -> TIntTy }
  bool                          { \s -> TBoolTy }
  true                          { \s -> TBool True }
  false                         { \s -> TBool False }
  $digit+                       { \s -> TInt (read s) }
  \=                            { \s -> TEq }
  \:                            { \s -> TColon }
  \,                            { \s -> TComma }
  \.                            { \s -> TDot }
  \#                            { \s -> TSharp }
  \\                            { \s -> TLam }
  \-\>                          { \s -> TArr }
  \+                            { \s -> TPlus }
  \-                            { \s -> TMinus }
  \*                            { \s -> TMult }
  \/                            { \s -> TDiv }
  \(                            { \s -> TLParen }
  \)                            { \s -> TRParen }
  \[                            { \s -> TLSquare }
  \]                            { \s -> TRSquare }
  \{                            { \s -> TLCurly }
  \}                            { \s -> TRCurly }
  \&                            { \s -> TAnd }
  \,\,                          { \s -> TMerge }
  $alpha [$alpha $digit \_ \']* { \s -> TStr s }

{
data Token = TLet
           | TIn
           | TIntTy
           | TInt Int
           | TBoolTy
           | TBool Bool
           | TStr String
           | TEq
           | TLam
           | TArr
           | TColon
           | TComma
           | TDot
           | TPlus
           | TMinus
           | TMult
           | TDiv
           | TLParen
           | TRParen
           | TLSquare
           | TRSquare
           | TLCurly
           | TRCurly
           | TSharp
           | TAnd
           | TMerge
           deriving (Eq, Show)

scanTokens = alexScanTokens
}
