module Interp where

import Grammars
import Desugar

instance Show ASA where
   show (Num n) = show n
   show (Fun p c) = "#<procedure>"

interp :: ASA -> ASA
interp (Num n)    = (Num n)
interp (Id i)     = error "Variable no definida."
interp (Suma i d) = Num ((numVn (interp i)) + (numVn (interp d)))
interp (Resta i d) = Num ((numVn (interp i)) - (numVn (interp d)))
interp (Mult i d) = Num ((numVn (interp i)) * (numVn (interp d)))
interp (Div i d)  = Num (div (numVn (interp i)) (numVn (interp d)))
interp (Fun p c)  = Fun p c
interp (App f a) = let fv = (interp f) in (interp (sust (body fv) (param fv) (interp a)))

numVn :: ASA -> Int
numVn (Num n) = n

param :: ASA -> String
param (Fun p c) = p

body :: ASA -> ASA
body (Fun p c) = c

sust :: ASA -> String -> ASA -> ASA
sust (Num n) id val = (Num n)
sust (Id i) id val = if (id == i) then val else (Id i)
sust (Suma i d) id val = Suma (sust i id val) (sust d id val)
sust (Resta i d) id val = Resta (sust i id val) (sust d id val)
sust (Mult i d) id val = Mult (sust i id val) (sust d id val)
sust (Div i d) id val = Div (sust i id val) (sust d id val)
sust (App f a) id val = App (sust f id val) (sust a id val)
sust (Fun p c) id val
   | id == p = Fun p c
   | id /= p = Fun p (sust c id val)