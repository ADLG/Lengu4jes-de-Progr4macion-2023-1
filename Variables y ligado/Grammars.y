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

%%

ASA : var                             { Id $1 }
    | int                             { Num $1 }
    | '(' '+' ASA ASA ')'             { Suma $3 $4}
    | '(' '-' ASA ASA ')'             { Resta $3 $4}
    | '(' '*' ASA ASA ')'             { Mult $3 $4}
    | '(' '/' ASA ASA ')'             { Div $3 $4}
    | '(' let '(' var ASA ')' ASA ')' { Let $4 $5 $7}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Id String
          | Num Int
          | Suma ASA ASA
          | Resta ASA ASA
          | Mult ASA ASA
          | Div ASA ASA
          | Let String ASA ASA
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