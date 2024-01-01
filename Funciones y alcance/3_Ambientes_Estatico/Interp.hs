module Interp where

import Grammars
import Desugar

instance Show Value where
   show (NumV n) = show n
   show (ClosureV p c e) = "#<procedure>"

type Env = [(String,Value)]

data Value = NumV Int
           | ClosureV String ASA Env

busca :: Env -> String -> Value
busca [] _ = error "Variable no definida."
busca ((i,v):xs) s = if i==s then v else busca xs s 

interp :: ASA -> Env -> Value
interp (Num n) env    = (NumV n)
interp (Id i)  env   = busca env i
interp (Suma i d) env = NumV ((numVn (interp i env)) + (numVn (interp d env)))
interp (Resta i d) env = NumV ((numVn (interp i env)) - (numVn (interp d env)))
interp (Mult i d) env = NumV ((numVn (interp i env)) * (numVn (interp d env)))
interp (Div i d) env = NumV (div (numVn (interp i env)) (numVn (interp d env)))
interp (Fun p c) env  = ClosureV p c env
interp (App f a) env = let fv = (interp f env) in (interp (body fv) ((param fv, (interp a env)):(enviroment fv)))

numVn :: Value -> Int
numVn (NumV n) = n

param :: Value -> String
param (ClosureV p c e) = p

body :: Value -> ASA
body (ClosureV p c e) = c

enviroment :: Value -> Env
enviroment (ClosureV p c e) = e