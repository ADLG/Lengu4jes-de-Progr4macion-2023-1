module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = show b

interp :: ASA -> Value
interp (Num n) = NumV n
interp (Boolean b) = BoolV b
interp (Op s l) = aplica s (map interp l)

aplica :: String -> [Value] -> Value
aplica "+" l = NumV (foldr1 (+) (map extraeI l))
aplica "-" l = NumV (foldr1 (-) (map extraeI l))
aplica "*" l = NumV (foldr1 (*) (map extraeI l))
aplica "/" l = NumV (division (map extraeI l))
aplica "<" l = BoolV (multiparam "<" (map extraeI l))
aplica ">" l = BoolV (multiparam ">" (map extraeI l))
aplica "=" l = BoolV (multiparam "=" (map extraeI l))

aplica "add1" [x] = NumV ((extraeI x) + 1) 
aplica "sub1" [x] = NumV ((extraeI x) - 1) 
aplica "not" [x] = if (extraeB x) then (BoolV False) else (BoolV True)
aplica "and" x = BoolV (foldr1 (&&) (map extraeB x))
aplica "or" x = BoolV (foldr1 (||) (map extraeB x))

multiparam :: String -> [Int] -> Bool
multiparam _ [] = True
multiparam _ [x] = True
multiparam "<" (x:y:xs) = (x < y) && (multiparam "<" (y:xs))
multiparam ">" (x:y:xs) = (x > y) && (multiparam ">" (y:xs))
multiparam "=" (x:y:xs) = (x == y) && (multiparam "=" (y:xs))

division :: [Int] -> Int
division [x] = x
division (x:y:xs) = if y == 0 then error "Division por cero" else (division (c:xs))
    where c = div x y

extraeI :: Value -> Int
extraeI (NumV n) = n

extraeB :: Value -> Bool
extraeB (BoolV b) = b 

