{
module Tokens where

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  let                           { \_ -> TLet }
  in                            { \_ -> TIn }
  fix                           { \_ -> TFix }
  int                           { \_ -> TIntTy }
  bool                          { \_ -> TBoolTy }
  true                          { \_ -> TBool True }
  false                         { \_ -> TBool False }
  if                            { \_ -> TIf }
  then                          { \_ -> TThen }
  else                          { \_ -> TElse }
  $digit+                       { \s -> TInt (read s) }
  \=                            { \_ -> TEq }
  \:                            { \_ -> TColon }
  \,                            { \_ -> TComma }
  \.                            { \_ -> TDot }
  \#                            { \_ -> TSharp }
  \\                            { \_ -> TLam }
  \-\>                          { \_ -> TArr }
  \\\/                          { \_ -> TForall }
  \/\\                          { \_ -> TDLam }
  \+                            { \_ -> TAdd }
  \-                            { \_ -> TSub }
  \*                            { \_ -> TMul }
  \/                            { \_ -> TDiv }
  \=\=                          { \_ -> TEqu }
  \!\=                          { \_ -> TNeq }
  \<                            { \_ -> TLt }
  \>                            { \_ -> TGt }
  \(                            { \_ -> TLParen }
  \)                            { \_ -> TRParen }
  \[                            { \_ -> TLSquare }
  \]                            { \_ -> TRSquare }
  \{                            { \_ -> TLCurly }
  \}                            { \_ -> TRCurly }
  \&                            { \_ -> TAnd }
  \,\,                          { \_ -> TMerge }
  \.\_                          { \_ -> TProj }
  \@                            { \_ -> TAt }
  T                             { \_ -> TTop }
  $alpha [$alpha $digit \_ \']* { \s -> TStr s }

{
data Token = TLet | TIn
           | TIntTy | TInt Int
           | TBoolTy | TBool Bool
           | TStr String
           | TEq
           | TLam
           | TForall
           | TDLam
           | TArr
           | TColon | TComma | TDot
           | TAdd | TSub | TMul | TDiv | TEqu | TNeq | TLt | TGt
           | TLParen | TRParen | TLSquare | TRSquare | TLCurly | TRCurly
           | TSharp
           | TAnd | TMerge
           | TIf | TThen | TElse
           | TProj
           | TTop
           | TAt
           | TFix
           deriving (Eq, Show)

scanTokens = alexScanTokens
}
