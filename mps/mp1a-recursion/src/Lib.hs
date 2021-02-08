--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake : takes the first n elements of a list, or the whole list if there are not n elements
-- help from http://learnyouahaskell.com/recursion
mytake :: Int -> [a] -> [a]
mytake n _
    | n <= 0 = []
mytake _ [] = []
mytake n (x:xs) = x : mytake (n-1) xs


--- ### mydrop
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs)
    | n > 0        = mydrop(n-1) xs
    | otherwise    = x:xs

--- ### rev
-- from haskell source code https://downloads.haskell.org/~ghc/6.12.1/docs/html/libraries/base-4.2.0.0/src/GHC-List.html#reverse
rev :: [a] -> [a]
rev = P.foldl (flip (:)) []

--- ### app
app :: [a] -> [a] -> [a] 
app [] b = b
app (x:xs) ys = x: (app xs ys)

--- ### inclist
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1):inclist xs

--- ### sumlist
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist [x] = x
sumlist(x:xs) = x + sumlist xs

--- ### myzip
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.List.html#zip
myzip :: [a] -> [b] -> [(a,b)]
myzip [] b = []
myzip a [] = []
myzip (a:as) (b:bs) = (a,b):myzip as bs

--- ### addpairs
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs = undefined
--addpairs [] = 0
--d = myzip xs ys
--addpairs (xs) (ys) = b where b = x + y where myzip 
-- addpairs xs ys = sum $ myzip xs ys

--- ### ones
-- help from https://www.techrepublic.com/article/infinite-list-tricks-in-haskell/
ones :: [Integer]
ones = 1 : P.map (+0) ones

--- ### nats
nats :: [Integer]
nats = 0 : P.map (+1) nats

--- ### fib
fib :: [Integer]
fib = undefined

--- Set Theory
--- ----------

--- ### add
add :: Ord a => a -> [a] -> [a]
add = undefined

--- ### union
union :: Ord a => [a] -> [a] -> [a]
union = undefined

--- ### intersect
intersect :: Ord a => [a] -> [a] -> [a]
intersect = undefined

--- ### powerset
powerset :: Ord a => [a] -> [[a]]
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'
inclist' :: Num a => [a] -> [a]
inclist' = undefined

--- ### sumlist'
sumlist' :: (Num a) => [a] -> a 
sumlist' = undefined
