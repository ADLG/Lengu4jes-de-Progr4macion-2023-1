{
module Grammars where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      int             { TokenNum $$ }
      bool            { TokenBool $$ }
      op              { TokenOp $$ }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : int               { Num $1 }
    | bool              { Boolean $1 }
    | '(' op LASA ')'   { Op $2 $3 }

LASA : ASA              { [$1] }
     | ASA LASA         { $1:$2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Num Int
         | Boolean Bool
         | Op String [ASA]
          deriving(Show)

data Token = TokenNum Int
           | TokenBool Bool
           | TokenOp String
           | TokenPA
           | TokenPC
           deriving(Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ':xs) = lexer xs
lexer ('(':xs) = TokenPA:(lexer xs)
lexer (')':xs) = TokenPC:(lexer xs)
lexer ('+':xs) = (TokenOp "+"):(lexer xs)
lexer ('-':xs) = (TokenOp "-"):(lexer xs)
lexer ('*':xs) = (TokenOp "*"):(lexer xs)
lexer ('/':xs) = (TokenOp "/"):(lexer xs)
lexer ('<':xs) = (TokenOp "<"):(lexer xs)
lexer ('>':xs) = (TokenOp ">"):(lexer xs)
lexer ('=':xs) = (TokenOp "="):(lexer xs)
lexer ('a':'d':'d':'1':xs) = (TokenOp "add1"):(lexer xs)
lexer ('s':'u':'b':'1':xs) = (TokenOp "sub1"):(lexer xs)
lexer ('n':'o':'t':xs) = (TokenOp "not"):(lexer xs)
lexer ('a':'n':'d':xs) = (TokenOp "and"):(lexer xs)
lexer ('o':'r':xs) = (TokenOp "or"):(lexer xs)
lexer ('t':'r':'u':'e':xs) = (TokenBool True):(lexer xs)
lexer ('f':'a':'l':'s':'e':xs) = (TokenBool False):(lexer xs)
lexer (x:xs)
    | isDigit x = lexNum(x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num): lexer rest
      where (num,rest) = span isDigit cs

main = getContents >>= print . parse . lexer

}
