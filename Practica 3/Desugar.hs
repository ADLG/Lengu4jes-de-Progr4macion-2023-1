module Desugar where

import Grammars

type Binding = (String,ASA)

data ASA = Id String
         | Num Int
         | Boolean Bool
         | Uop String ASA
         | Binop String ASA ASA
         | Let [Binding] ASA
          deriving(Show)

desugar :: ASAS -> ASA
desugar (IdS i) = (Id i)
desugar (NumS n) = (Num n)
desugar (BooleanS n) = (Boolean n)
desugar (OpS s (x:xs)) = app s (map desugar (x:xs))
desugar (LetS [(i,c)] d) = Let [(i,(desugar c))] (desugar d)

app :: String -> [ASA] -> ASA
app "+" l = Num (foldr1 (+) (map extrae l))
app "-" l = Num (foldr1 (-) (map extrae l))
app "*" l = Num (foldr1 (*) (map extrae l))
app "/" l = Num (divi (map extrae l))
app "<" l = Boolean (multiparamDs "<" (map extrae l))
app ">" l = Boolean (multiparamDs ">" (map extrae l))
app "=" l = Boolean (multiparamDs "=" (map extrae l))

app "add1" [x] = Num ((extrae x) + 1) 
app "sub1" [x] = Num ((extrae x) - 1) 

app "not" [x] = if (extraeBB x) then (Boolean False) else (Boolean True)
app "and" x = Boolean (foldr1 (&&) (map extraeBB x))
app "or" x = Boolean (foldr1 (||) (map extraeBB x))

multiparamDs :: String -> [Int] -> Bool
multiparamDs _ [] = True
multiparamDs _ [x] = True
multiparamDs "<" (x:y:xs) = (x < y) && (multiparamDs "<" (y:xs))
multiparamDs ">" (x:y:xs) = (x > y) && (multiparamDs ">" (y:xs))
multiparamDs "=" (x:y:xs) = (x == y) && (multiparamDs "=" (y:xs))

divi :: [Int] -> Int
divi [x] = x
divi (x:y:xs) = if y == 0 then error "Division por cero" else (divi (c:xs))
    where c = div x y

extrae :: ASA -> Int
extrae (Num n) = n

extraeBB :: ASA -> Bool
extraeBB (Boolean b) = b 