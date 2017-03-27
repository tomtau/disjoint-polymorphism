{
module Source.Parser (parseExpr) where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Source.Syntax
import Unbound.LocallyNameless
import Common
import Lexer
import PrettyPrint
import Source.SrcLoc
}


%name parser
%tokentype { Token }
%monad { Alex } { (>>=) } { return }
%lexer { lexer } { T _ TEOF "" }
%error { parseError }


%token

    def         { T _ TKey "def" }
    defrec      { T _ TKey "defrec" }
    trait       { T _ TKey "trait" }
    val         { T _ TKey "val" }
    Trait       { T _ TKey "Trait" }
    inherits    { T _ TKey "inherits" }
    new         { T _ TKey "new" }
    main        { T _ TKey "main" }
    forall      { T _ TKey "forall" }
    typ         { T _ TKey "type" }
    let         { T _ TKey "let" }
    in          { T _ TKey "in" }
    module      { T _ TKey "module" }
    double      { T _ TKey "Double" }
    bool        { T _ TKey "Bool" }
    boolV       { T _ (TBool $$) _ }
    num         { T _ (TNum $$) _ }
    string      { T _ TKey "String" }
    str         { T _ (TStr $$) _ }
    if          { T _ TKey "if" }
    then        { T _ TKey "then" }
    else        { T _ TKey "else" }
    ':'         { T _ TSym ":" }
    '='         { T _ TSym "=" }
    '.'         { T _ TSym "." }
    ','         { T _ TSym "," }
    '{'         { T _ TSym "{" }
    '}'         { T _ TSym "}" }
    '['         { T _ TSym "[" }
    ']'         { T _ TSym "]" }
    '->'        { T _ TSym "->" }
    '('         { T _ TSym "(" }
    ')'         { T _ TSym ")" }
    lam         { T _ TSym "\\" }
    blam        { T _ TSym "/\\" }
    '&'         { T _ TSym "&" }
    ',,'        { T _ TSym ",," }
    top         { T _ TSym "()" }
    topT        { T _ TSym "T" }
    '+'         { T _ TSym "+" }
    '-'         { T _ TSym "-" }
    '*'         { T _ TSym "*" }
    '/'         { T _ TSym "/" }
    '<'         { T _ TSym "<" }
    '>'         { T _ TSym ">" }
    '=='        { T _ TSym "==" }
    '/='        { T _ TSym "/=" }
    ';'         { T _ TSym ";" }
    '_'         { T _ TSym "_" }
    '=>'        { T _ TSym "=>" }
    '++'        { T _ TSym "++" }


    LOWER_IDENT { T _ (Tlowerid $$) _ }
    UPPER_IDENT { T _ (Tupperid $$) _ }

%right '->'
%left ',,'
%nonassoc '==' '/='
%nonassoc '<' '>'
%left '+' '-' '++'
%left '*' '/'



%%


------------------------------------------------------------------------
-- Programs
------------------------------------------------------------------------

prog :: { Module }
  : decllist main_or_unit   { Module $1 $2 }
  -- Only for REPL
  | expr                     { Module [] (DefDecl (TmBind "main" [] [] $1 Nothing)) }

traitdecl :: { Trait }
  : trait LOWER_IDENT ctyparam_list trait_params_list inherit ret_type
  { TraitDef $2 ("self", TopT) $5 $6 (map (\(n, b) -> (s2n n, b)) $3) (map (\(n, b) -> (s2n n, b)) $4) [] }
  | trait LOWER_IDENT ctyparam_list trait_params_list inherit ret_type '{' traitbody '}'
  { TraitDef $2 (fst $8, fst (snd $8)) $5 $6 (map (\(n, b) -> (s2n n, b)) $3) (map (\(n, b) -> (s2n n, b)) $4) (snd (snd $8)) }

traitbody :: { (String, (Type, [SimpleDecl])) }
  : LOWER_IDENT ':' type '=>' sdecllist     { ($1, ($3, $5)) }
  | LOWER_IDENT '=>' sdecllist              { ($1, (TopT, $3)) }

ret_type :: { Maybe Type }
  : {- empty -}     { Nothing }
  | ':' type        { Just $2 }

inherit :: { [Expr] }
  : {- empty -}            { [] }
  | inherits traitConstrs  { $2 }

decllist :: { [Decl] }
  : {- empty -}       { [] }
  | decl decllist     { $1 : $2}

decl :: { Decl }
  : sdecl     { SDecl $1 }
  | traitdecl { TraitDecl $1 }

main_or_unit :: { SimpleDecl }
  : {- empty -}              { DefDecl (TmBind "main" [] [] Top Nothing) }
  | main '=' expr            { DefDecl (TmBind "main" [] [] $3 Nothing) }


sdecllist :: { [SimpleDecl] }
  : {- empty -}         { [] }
  | sdecllist1          { $1 }

sdecllist1 :: { [SimpleDecl] }
  : sdecl         { [$1] }
  | sdecl  sdecllist1 { $1 : $2 }

sdecl :: { SimpleDecl }
  : def bind                  { DefDecl $2 }
  | val bind                  { DefDecl $2 }
  | typ typebind              { TypeDecl $2 }
  | defrec recbind            { DefDecl $2 }


bind :: { TmBind }
  : LOWER_IDENT ctyparam_list param_list ret_type '=' expr
  { TmBind $1 (map (\(n, b) -> (s2n n, b)) $2) (map (\(n, b) -> (s2n n, b)) $3) $6 $4 }

recbind :: { TmBind }
  : LOWER_IDENT ctyparam_list param_list ':' type '=' expr
  { let (typ, trm) = teleToTmBind $2 $3 $5 $7
    in TmBind $1 [] [] (elet $1 typ trm (evar $1)) (Just typ)
  }


typebind :: { TypeBind }
  : UPPER_IDENT  typaram_list '=' type     { TypeBind $1 $2 $4 }

traitConstrs :: { [Expr] }
  : traitConstr                      { [$1] }
  | traitConstr '&' traitConstrs     { $1:$3 }


traitConstr :: { Expr }
  : LOWER_IDENT type_list_or_empty  args                    { App (foldl App (foldl TApp (evar $1) $2) $3) (evar "self") }
  | LOWER_IDENT type_list_or_empty  args lam LOWER_IDENT    { Remove (App (foldl App (foldl TApp (evar $1) $2) $3) (evar "self")) $5 }


type_list_or_empty :: { [Type] }
  : {- empty -}              { [] }
  | '[' comma_types1 ']'     { $2 }


-- Parse "(x, y, z)" or "() or nothing at all
args :: { [Expr] }
  : {- empty -}        { [] }
  | top                { [Top] }
  | '(' arglist ')'    { $2 }

arglist :: { [Expr] }
  : expr                   { [$1] }
  | expr ',' arglist       { $1:$3 }


-- Parse "(x : Int, y : Bool)" or "()" or nothing at all
trait_params_list :: { [(String, Type)] }
  : {- empty -}           { [] }
  | top                   { [("_", TopT)] }
  | '(' tparams ')'         { $2 }

tparams :: { [(String, Type)] }
      : tparam                 { [$1] }
      | tparam ',' tparams       { $1:$3 }

tparam : LOWER_IDENT ':' type  { ($1, $3) }



------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type :: { Type }
  : forall ctyparams1 '.' type  { foldr tforall (tforall (last $2) $4) (init $2) }
  | monotype                     { $1 }

monotype :: { Type }
  : intertype '->' monotype  { Arr $1 $3 }
  | intertype                { $1 }

-- & is left associative
intertype :: { Type }
  : intertype '&' ftype      { And $1 $3 }
  | ftype                    { $1 }

ftype :: { Type }
  : ftype type_list  { foldl OpApp (OpApp $1 (head $2)) (tail $2) }
  | atype            { $1 }

type_list :: { [Type] }
  : '[' comma_types1 ']'     { $2 }

comma_types1 :: { [Type] }
  : type                   { [$1]  }
  | type ',' comma_types1  { $1:$3 }

atype :: { Type }
  : double                           { NumT }
  | bool                          { BoolT }
  | string                        { StringT }
  | topT                          { TopT }
  | record_type                   { $1 }
  | '(' type ')'                  { $2 }
  | Trait '[' type ',' type ']'   { Arr $3 $5 }
  | Trait '[' type ']'            { Arr TopT $3 }
  | UPPER_IDENT                   { tvar $1 }

-- record types
record_type :: { Type }
  : '{' record_type_fields_rev '}'      { mkRecdsT (reverse $2) }

-- Happy is more efficient at parsing left-recursive rules!
record_type_fields_rev :: { [(Label, Type)] }
  : record_type_field                             { [$1]  }
  | record_type_fields_rev ',' record_type_field  { $3:$1 }

record_type_field :: { (Label, Type) }
  : label ':' type                                { ($1, $3) }


{-------------------------------------------------------------------------------
        Type parameters
-------------------------------------------------------------------------------}

typaram :: { (TyName, Kind) }
  : UPPER_IDENT                                 { (s2n $1, Star) }
  | UPPER_IDENT '[' underscores ']'             { (s2n $1, $3) }

underscores :: { Kind }
  : '_'                          { KArrow Star Star }
  | '_' ',' underscores          { KArrow Star $3 }

typarams :: { [(TyName, Kind)] }
  : {- empty -}                    { []    }
  | typaram typarams               { $1:$2 }

-- typarams1 :: { [TyName] }
--   : typaram typarams               { $1:$2 }

typaram_list :: { [(TyName, Kind)] }
  : '[' comma_typarams1 ']'        { $2 }
  | {- empty -}                    { [] }

comma_typarams1 :: { [(TyName, Kind)] }
  : typaram                        { [$1]  }
  | typaram ',' comma_typarams1    { $1:$3 }


{-------------------------------------------------------------------------------
        constrainable type parameters
-------------------------------------------------------------------------------}

ctyparam :: { (String, Type) }
  : UPPER_IDENT              { ($1, TopT) }
  | '[' UPPER_IDENT '*' type ']'     { ($2, $4) }

ctyparam_list1 :: { [(String, Type)] }
  : ctyparam                 { [$1]  }
  | ctyparam ctyparam_list1  { $1:$2 }

ctyparam_list :: { [(String, Type)] }
  : ctyparam_list1        { $1 }
  | {- empty -}           { [] }

paren_ctyparam :: { (String, Type) }
  : UPPER_IDENT                     { ($1, TopT) }
  | '(' UPPER_IDENT '*' type ')'    { ($2, $4) }

ctyparams :: { [(String, Type)] }
  : {- empty -}                     { []    }
  | paren_ctyparam ctyparams        { $1:$2 }

ctyparams1 :: { [(String, Type)] }
  : paren_ctyparam ctyparams        { $1:$2 }


------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: { Expr }
     : lam lparam_list1 '->' expr                            { foldr elam (elam (last $2) $4) (init $2) }
     | blam ctyparams1 '.' expr                              { foldr dlam (dlam (last $2) $4) (init $2) }
     | let LOWER_IDENT ':' type '=' expr in expr             { elet $2 $4 $6 $8 }
     | if expr then expr else expr                           { If $2 $4 $6 }
     | new '[' type ']' traitConstrs                         { transNew $3 $5 }
     | infixexpr                                             { $1 }

infixexpr :: { Expr }
          : infixexpr '+' infixexpr                                       { PrimOp (Arith Add) $1 $3 }
          | infixexpr '-' infixexpr                                       { PrimOp (Arith Sub) $1 $3 }
          | infixexpr '*' infixexpr                                       { PrimOp (Arith Mul) $1 $3 }
          | infixexpr '/' infixexpr                                       { PrimOp (Arith Div) $1 $3 }
          | infixexpr '==' infixexpr                                      { PrimOp (Logical Equ) $1 $3 }
          | infixexpr '/=' infixexpr                                      { PrimOp (Logical Neq) $1 $3 }
          | infixexpr '<' infixexpr                                       { PrimOp (Logical Lt) $1 $3 }
          | infixexpr '>' infixexpr                                       { PrimOp (Logical Gt) $1 $3 }
          | infixexpr '++' infixexpr                                      { PrimOp Append $1 $3 }
          | infixexpr ',,' infixexpr                                      { Merge $1 $3 }
          | fexpr                                                         { $1 }


fexpr :: { Expr }
      : fexpr aexpr      { App  $1 $2 }
      | fexpr type       { TApp $1 $2 }
      | aexpr            { $1 }

aexpr :: { Expr }
      : LOWER_IDENT                      { evar $1 }
      | num                              { LitV $1 }
      | boolV                            { BoolV $1 }
      | str                              { StrV $1 }
      | aexpr '.' LOWER_IDENT            { Acc $1 $3 }
      | record_construct                 { $1 }
      | aexpr lam LOWER_IDENT            { Remove $1 $3 }
      | top                              { Top }
      | aexpr ':' type                   { Anno $1 $3 }
      | '(' expr ')'                     { $2 }

-- record-construction expr
record_construct :: { Expr }
  : '{' record_construct_fields_rev '}'                     { mkRecds (reverse $2) }

record_construct_fields_rev :: { [(Label, Expr)] }
  : record_construct_field                                  { [$1]  }
  | record_construct_fields_rev ',' record_construct_field  { $3:$1 }

record_construct_field :: { (Label, Expr) }
  : label '=' expr                                          { ($1, $3) }


-- Parse "(x : Int) (y : Int)  z" or "()" or nothing at all
param_list :: { [(String, Maybe Type)] }
  : params                    { $1 }
  | top                       { [("_", Just TopT)] }

param_list1 :: { [(String, Maybe Type)] }
  : params1                   { $1 }

param :: { (String, Maybe Type) }
  : '(' LOWER_IDENT ':' type ')'    { ($2, Just $4) }
  | '(' '_' ':' type ')'            { ("_", Just $4) }
  | LOWER_IDENT                     { ($1, Nothing) }

params :: { [(String, Maybe Type)] }
  : {- empty -}  { []    }
  | param params { $1:$2 }

params1 :: { [(String, Maybe Type)] }
  : param params { $1:$2 }


-- Parse "x y z" or noting at all
lparam_list :: { [String] }
  : lparams                    { $1 }

lparam_list1 :: { [String] }
  : lparams1                   { $1 }

lparam :: { String }
  : LOWER_IDENT  { $1 }
  | '_'           { "_" }

lparams :: { [String] }
  : {- empty -}  { []    }
  | lparam lparams { $1:$2 }

lparams1 :: { [String] }
  : lparam lparams { $1:$2 }


-- Misc
------------------------------------------------------------------------

ident :: { String }
  : UPPER_IDENT  { $1 }
  | LOWER_IDENT  { $1 }

label :: { Label }
  : LOWER_IDENT  { $1 }


{

parseError :: Token -> Alex a
parseError (T p _ s) =
  alexError (showPosn p ++ ": parse error at token '" ++ s ++ "'")

parseExpr :: String -> Either String Module
parseExpr inp = runAlex inp parser

}
