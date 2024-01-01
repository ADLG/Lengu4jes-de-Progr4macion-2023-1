module Desugar where

import Grammars

data ASA = Id String
         | Num Int
         | Suma ASA ASA
         | Resta ASA ASA
         | Mult ASA ASA
         | Div ASA ASA
         | Fun String ASA
         | App ASA ASA

desugar :: ASAS -> ASA
desugar (IdS i) = Id i
desugar (NumS n) = Num n
desugar (SumaS i d) = Suma (desugar i) (desugar d)
desugar (RestaS i d) = Resta (desugar i) (desugar d)
desugar (MultS i d) = Mult (desugar i) (desugar d)
desugar (DivS i d) = Div (desugar i) (desugar d)
desugar (LetS i v c) = App (Fun i (desugar c)) (desugar v)
desugar (FunS p c) = Fun p (desugar c)
desugar (AppS f a) = App (desugar f) (desugar a)
