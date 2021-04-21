--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

-- direct style:
-- factk a = a * factk a-1
factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk a k = factk (a - 1) (\v -> k $ v * a)


--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] kev kodd
    | odd x    = kodd x
    | otherwise = kev x
evenoddk (x:xs) kev kodd
    | odd x     = evenoddk xs kev (\v -> kodd $ v + x)
    | otherwise = evenoddk xs (\v -> kev $ v + x) kodd

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`
--          IntExp Integer
--          | VarExp String
--          | LamExp String Exp
--          | IfExp Exp Exp Exp
--          | OpExp String Exp Exp
--          | AppExp Exp Exp
isSimple :: Exp -> Bool
isSimple (IntExp _)     = True
isSimple (VarExp _)     = True
isSimple (IfExp e1 e2 e3)  = isSimple e1 && isSimple e2 && isSimple e3
isSimple (OpExp s e1 e2)  = isSimple e1 && isSimple e2
isSimple (AppExp e1 e2 )  = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp e) k i = (AppExp k (IntExp e),i)
cpsExp (VarExp e) k i = (AppExp k (VarExp e), i)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp e1 e2) k i
    | isSimple e2   = (AppExp (AppExp e1 e2) k, i)
    | otherwise     = cpsExp e2 (LamExp v (AppExp(AppExp e1 (VarExp v)) k)) i' 
                        where   (v, i') = gensym i

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp s e1 e2) k i
    | isSimple e1 && isSimple e2             = (AppExp k (OpExp s e1 e2), i)
    | isSimple e1 && not (isSimple e2)       = cpsExp e2 (LamExp v (AppExp k (OpExp s e1 (VarExp v)))) i'
    | not (isSimple e1) && isSimple e2       = cpsExp e2 (LamExp v (AppExp k (OpExp s e1 (VarExp v)))) i'
    | not (isSimple e1) && not (isSimple e2) = cpsExp e1 (LamExp v k') i'''
                                                where   (v, i')     = gensym i
                                                        (v', i'')   = gensym i'
                                                        (k', i''')   = cpsExp e2 (LamExp v' (AppExp k (OpExp s (VarExp v) (VarExp v')))) i''

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k i
    | isSimple e1   = (IfExp e1 e2' e3', i)
    | otherwise     = cpsExp e1 (LamExp v (IfExp (VarExp v) e2' e3')) i'
                        where   (v, i') = gensym i
                                (e2', _) = cpsExp e2 k i
                                (e3', _) = cpsExp e3 k i
    -- isSimple e1 && not isSimple e2
--- ### Define `cpsDecl`
-- Decl String [String] Exp
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f xx e) = Decl f (xx ++ ["k"]) k'
                            where   (k', _) = cpsExp e (VarExp "k") 1
