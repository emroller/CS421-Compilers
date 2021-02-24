--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

--

cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x:cons2list xs

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x) = x
eval (PlusExp x) = sum (map eval x)
eval (MultExp x) =  product (map eval x)

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' = foldr Cons Nil

--- ### BinTree

-- BinTree
data BinTree a =  Leaf 
                | Node a (BinTree a) (BinTree a) 
  deriving (Show, Read, Eq)  

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a 
sumTree Leaf = 0
sumTree (Node p l r) = p + sumTree l + sumTree r

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal  Integer
             | BoolVal Bool
             | StrVal  String
             | ExnVal  String
    deriving (Show)

--- ### liftIntOp

liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal x) (IntVal y)  = IntVal (f x y)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
