{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Prop where

import PicoC


--------------------------------------------------------
-- Eval Function
--------------------------------------------------------

-- eval ast [("aux1", 10)]
-- eval (GreaterEqual (Add (Const 3) (Const 5)) (Sub (Const 10) (Var "aux"))) [("aux",2)] -- True
evalPicoc :: PicoC -> [(String,Int)] -> [(String,Int)]

evalInst :: Inst -> [(String,Int)] -> [(String,Int)]

evalExp :: Exp -> [(String,Int)] -> Int
evalExp (Add e1 e2) c = evalExp e1 c + evalExp e2 c
evalExp (Sub e1 e2) c = evalExp e1 c - evalExp e2 c
evalExp (Mult e1 e2) c = evalExp e1 c * evalExp e2 c
evalExp (Div e1 e2) c = evalExp e1 c `div` evalExp e2 c
evalExp (Neg e) c = - (evalExp e c)
evalExp (Const i) _ = i
evalExp (Var n) c = fromJust (lookup n c)                            -- lookup vai buscar o valor da variável no contexto c; fromjust coverte aquilo que o lookup devolve
evalExp (Boolean b) c = fromEnum $ evalExp b c
evalExp (Equal e1 e2) c = fromEnum $ evalExp e1 c == evalExp e2 c    -- fromEnum converte um booleano para um inteiro
evalExp (Greater e1 e2) c = fromEnum $ evalExp e1 c > evalExp e2 c
evalExp (Less e1 e2) c = fromEnum $ evalExp e1 c < evalExp e2 c
evalExp (GreaterEqual e1 e2) c = fromEnum $ evalExp e1 c >= evalExp e2 c
evalExp (LessEqual e1 e2) c = fromEnum $ evalExp e1 c <= evalExp e2 c
evalExp (And e1 e2) c = fromEnum $ evalExp e1 c /= 0 && evalExp e2 c /= 0
evalExp (Or e1 e2) c = fromEnum $ evalExp e1 c /= 0 || evalExp e2 c /= 0
evalExp (Not e) c = fromEnum $ evalExp e c == 0