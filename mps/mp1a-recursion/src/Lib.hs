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

-- don't forget to put the type declaration or you will lose points!
inclist = undefined

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist = undefined

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip = undefined

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs = undefined

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones = undefined

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats = undefined

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib = undefined

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add = undefined

--- ### union

-- don't forget to put the type declaration or you will lose points!
union = undefined

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect = undefined

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' = undefined

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' = undefined
