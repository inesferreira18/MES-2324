{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Prop where

import PicoC
import Data.Maybe
import Data.List
import Parser (ex)


--------------------------------------------------------
-- Eval Function
--------------------------------------------------------

--evaluate :: PicoC -> [a] -> Int
--evaluate prog inputs = evalFunc prog inputs

--evalFunc :: [Func] -> [a] -> Int
--evalFunc 


evalExp :: Exp -> [(String,Int)] -> Int
evalExp (Add e1 e2) c = evalExp e1 c + evalExp e2 c
evalExp (Sub e1 e2) c = evalExp e1 c - evalExp e2 c
evalExp (Mult e1 e2) c = evalExp e1 c * evalExp e2 c
evalExp (Div e1 e2) c = evalExp e1 c `div` evalExp e2 c
evalExp (Neg e) c = - (evalExp e c)
evalExp (Const i) _ = i
evalExp (Var n) c = fromJust (lookup n c)                            -- lookup vai buscar o valor da variável no contexto c; fromjust coverte aquilo que o lookup devolve
evalExp (Boolean b) _ = fromEnum b                                   -- AAAAAAAAAAAAa
evalExp (Equal e1 e2) c = fromEnum $ evalExp e1 c == evalExp e2 c    -- fromEnum converte um booleano para um inteiro
evalExp (Greater e1 e2) c = fromEnum $ evalExp e1 c > evalExp e2 c
evalExp (Less e1 e2) c = fromEnum $ evalExp e1 c < evalExp e2 c
evalExp (GreaterEqual e1 e2) c = fromEnum $ evalExp e1 c >= evalExp e2 c
evalExp (LessEqual e1 e2) c = fromEnum $ evalExp e1 c <= evalExp e2 c
evalExp (And e1 e2) c = fromEnum $ evalExp e1 c /= 0 && evalExp e2 c /= 0
evalExp (Or e1 e2) c = fromEnum $ evalExp e1 c /= 0 || evalExp e2 c /= 0
evalExp (Not e) c = fromEnum $ evalExp e c == 0

update :: (String, Int) -> [(String,Int)] -> [(String,Int)]
update (s,i) c = if isNothing (lookup s c) then insert (s,i) c else insert (s,i) (delete (s,fromJust (lookup s c)) c)

run :: [Inst] -> [(String,Int)] -> Int
run ((ITE exp b1 b2):t) c = if toEnum (evalExp exp c) then run (b1 ++ t) c else run (b2 ++ t) c
run (w@(While exp b):t) c = if toEnum (evalExp exp c) then run (b ++ [w] ++ t) c else run t c
run (f@(For init exp inc b):t) c = run (init ++ While exp (b++inc):t) c
run [Return exp] c = evalExp exp c
run ((Atrib a exp):t) c = run t (update (a,evalExp exp c) c)
run ((DeclAtrib tp a exp):t) c = run t (update (a, evalExp exp c) c)        -- tp dá problema?