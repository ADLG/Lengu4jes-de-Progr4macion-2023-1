module Interp where

import Grammars
import Desugar

instance Show ASA where
   show (Num n) = show n
   show (Fun p c) = "#<procedure>"

type Env = [(String,ASA)]

busca :: Env -> String -> ASA
busca [] _ = error "Variable no definida."
busca ((i,v):xs) s = if i==s then v else busca xs s 

interp :: ASA -> Env -> ASA
interp (Num n) env    = (Num n)
interp (Id i)  env   = busca env i
interp (Suma i d) env = Num ((numVn (interp i env)) + (numVn (interp d env)))
interp (Resta i d) env = Num ((numVn (interp i env)) - (numVn (interp d env)))
interp (Mult i d) env = Num ((numVn (interp i env)) * (numVn (interp d env)))
interp (Div i d) env = Num (div (numVn (interp i env)) (numVn (interp d env)))
interp (Fun p c) env  = Fun p c
interp (App f a) env = let fv = (interp f env) in (interp (body fv) ((param fv, (interp a env)):env))

numVn :: ASA -> Int
numVn (Num n) = n

param :: ASA -> String
param (Fun p c) = p

body :: ASA -> ASA
body (Fun p c) = c