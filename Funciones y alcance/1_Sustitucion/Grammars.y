{
module Grammars where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      var             { TokenId $$ }
      int             { TokenNum $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMult }
      '/'             { TokenDiv }
      '('             { TokenPA }
      ')'             { TokenPC }
      let             { TokenLet }
      lambda          { TokenLambda }

%%

ASAS : var                              { IdS $1 }
    | int                               { NumS $1 }
    | '(' '+' ASAS ASAS ')'             { SumaS $3 $4 }
    | '(' '-' ASAS ASAS ')'             { RestaS $3 $4 }
    | '(' '*' ASAS ASAS ')'             { MultS $3 $4 }
    | '(' '/' ASAS ASAS ')'             { DivS $3 $4 }
    | '(' let '(' var ASAS ')' ASAS ')' { LetS $4 $5 $7 }
    | '(' lambda '(' var ')' ASAS ')'   { FunS $4 $6 }
    | '(' ASAS ASAS ')'                 { AppS $2 $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASAS = IdS String
          | NumS Int
          | SumaS ASAS ASAS
          | RestaS ASAS ASAS
          | MultS ASAS ASAS
          | DivS ASAS ASAS
          | LetS String ASAS ASAS
          | FunS String ASAS
          | AppS ASAS ASAS
          deriving(Show)

data Token = TokenId String
           | TokenNum Int
           | TokenSuma
           | TokenResta
           | TokenMult
           | TokenDiv
           | TokenPA
           | TokenPC
           | TokenLet
           | TokenLambda
           deriving(Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ' : xs) = lexer xs
lexer ('(' : xs) = TokenPA:(lexer xs)
lexer (')' : xs) = TokenPC:(lexer xs)
lexer ('+' : xs) = TokenSuma:(lexer xs)
lexer ('-' : xs) = TokenResta:(lexer xs)
lexer ('*' : xs) = TokenMult:(lexer xs)
lexer ('/' : xs) = TokenDiv:(lexer xs)
lexer ('l':'e':'t':xs) = TokenLet:(lexer xs)
lexer ('l':'a':'m':'b':'d':'a':xs) = TokenLambda:(lexer xs)
lexer (x:xs)
    | isDigit x = lexNum (x:xs)
    | isAlpha x = lexAlph (x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexAlph :: String -> [Token]
lexAlph cs = TokenId var : lexer rest
      where (var,rest) = span isAlpha cs

main = getContents >>= print . parse . lexer

}