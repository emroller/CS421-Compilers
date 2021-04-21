module Lib
    ( Calc(..)
    , calc
    ) where

data Calc a = Add a
            | Sub a
   deriving (Eq,Show)

-- original:
-- calc xx init = aux init xx
--   where aux a [] = a
--         aux a ((Add i):xs) = aux (a+i) xs
--         aux a ((Sub i):xs) = aux (a-i) xs
        
calc :: Num a => [Calc a] -> a -> (a -> a) -> (a -> a) -> a
calc xx init ka ks = aux init xx ka ks
  where aux a [] ka ks =  ks (ka a)
        aux a ((Add i):xs) ka ks = aux a xs (\x-> ka (i + x)) ks
        aux a ((Sub i):xs) ka ks = aux a xs ka (\x-> ks (x - i))