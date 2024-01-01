module Interp where

import Grammars

interp :: ASA -> Int
interp (Num n)    = n
interp (Id i)     = error "Variable no definida."
interp (Suma i d) = (interp i) + (interp d)
interp (Resta i d) = (interp i) - (interp d)
interp (Mult i d) = (interp i) * (interp d)
interp (Div i d)  = div (interp i) (interp d)
interp (Let i val cuerpo) = interp (sust cuerpo i (Num (interp val)))

sust :: ASA -> String -> ASA -> ASA
sust (Num n) id val = (Num n)
sust (Id i) id val = if (id == i) then val else (Id i)
sust (Suma i d) id val = Suma (sust i id val) (sust d id val)
sust (Resta i d) id val = Resta (sust i id val) (sust d id val)
sust (Mult i d) id val = Mult (sust i id val) (sust d id val)
sust (Div i d) id val = Div (sust i id val) (sust d id val)
sust (Let i v c) id val
   | id == i = Let i (sust v id val) c
   | id /= i = Let i (sust v id val) (sust c id val)