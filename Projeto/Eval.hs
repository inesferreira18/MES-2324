{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Eval where

import PicoC
import Data.Maybe
import Data.List


--------------------------------------------------------
-- Runner Functions
--------------------------------------------------------

runTestSuite :: PicoC -> [([Int], Int)] -> Bool
runTestSuite p ts = all (runTest p) ts

runTest :: PicoC -> ([Int], Int) -> Bool
runTest p (inputs, expected) = evaluate p inputs == expected


--------------------------------------------------------
-- Eval Functions
--------------------------------------------------------

-- Funtion used to update the context with the new value of a variable
updateContext :: (String, Int) -> [(String,Int)] -> [(String,Int)]
updateContext (s,i) c = if isNothing (lookup s c) then insert (s,i) c else insert (s,i) (delete (s,fromJust (lookup s c)) c)


evaluate :: PicoC -> [Int] -> Int
evaluate (PicoC ((Func Int "main" args b):t)) = evalMain (Func Int "main" args b)

-- Evaluate the main function of the program (the remaining functions are evaluated in the context of the main function)
evalMain :: Func -> [Int] -> Int
evalMain (Func _  _ args b) inputs = evalInst b (zip (map (\(Arg _ s) -> s) args) inputs)  -- Create a context

evalFunc :: Func -> [Int] -> Int
evalFunc (Func tp _ args b) inputs = evalInst b (zip (map (\(Arg _ s) -> s) args) inputs)


evalInst :: [Inst] -> [(String,Int)] -> Int
evalInst ((ITE exp b1 b2):t) c = if toEnum (evalExp exp c) then evalInst (b1 ++ t) c else evalInst (b2 ++ t) c
evalInst (w@(While exp b):t) c = if toEnum (evalExp exp c) then evalInst (b ++ [w] ++ t) c else evalInst t c
evalInst (f@(For init exp inc b):t) c = evalInst (init ++ While exp (b++inc):t) c
evalInst [Return exp] c = evalExp exp c
evalInst ((Atrib a exp):t) c = evalInst t (updateContext (a,evalExp exp c) c)
evalInst ((DeclAtrib tp a exp):t) c = evalInst t (updateContext (a, evalExp exp c) c)
evalInst ((Decl tp name):t) c = evalInst t (updateContext (name,value) c)
                              where value = case tp of
                                            Int -> -1
                                            Char -> -1
                                            String -> -1
                                            Bool -> -1


evalExp :: Exp -> [(String,Int)] -> Int
evalExp (Add e1 e2) c = evalExp e1 c + evalExp e2 c
evalExp (Sub e1 e2) c = evalExp e1 c - evalExp e2 c
evalExp (Mult e1 e2) c = evalExp e1 c * evalExp e2 c
evalExp (Div e1 e2) c = evalExp e1 c `div` evalExp e2 c
evalExp (Neg e) c = - (evalExp e c)
evalExp (Const i) _ = i
evalExp (Boolean b) _ = fromEnum b                                   -- AAAAAAAAAAAAa
evalExp (Equal e1 e2) c = fromEnum $ evalExp e1 c == evalExp e2 c    -- fromEnum converte um booleano para um inteiro
evalExp (Greater e1 e2) c = fromEnum $ evalExp e1 c > evalExp e2 c
evalExp (Less e1 e2) c = fromEnum $ evalExp e1 c < evalExp e2 c
evalExp (GreaterEqual e1 e2) c = fromEnum $ evalExp e1 c >= evalExp e2 c
evalExp (LessEqual e1 e2) c = fromEnum $ evalExp e1 c <= evalExp e2 c
evalExp (And e1 e2) c = fromEnum $ evalExp e1 c /= 0 && evalExp e2 c /= 0
evalExp (Or e1 e2) c = fromEnum $ evalExp e1 c /= 0 || evalExp e2 c /= 0
evalExp (Not e) c = fromEnum $ evalExp e c == 0
evalExp (Var n) c = case lookup n c                            -- lookup vai buscar o valor da variável no contexto c; fromjust coverte aquilo que o lookup devolve
                    of Just i -> i
                       Nothing -> error ("Variable " ++ n ++ " not found!")