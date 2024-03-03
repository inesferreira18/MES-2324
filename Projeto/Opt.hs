module Opt where

import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Data.Maybe (Maybe (Just))

import PicoC

--------------------------------------------------------
-- Optimization Function
--------------------------------------------------------

-- Main function
opt :: PicoC -> PicoC
opt program = 
    let pProgram = toZipper program
        Just newProgram = applyTp (full_tdTP step) pProgram
        step = idTP `adhocTP` optNeutral `adhocTP` optAbsorbing `adhocTP` optNegative `adhocTP` optOperation `adhocTP` optEq  `adhocTP` optLoop `adhocTP` optCond
    in fromZipper newProgram

-- Neutral element optimization
optNeutral :: Exp -> Maybe Exp
optNeutral (Add (Const 0) e) = Just e
optNeutral (Add e (Const 0)) = Just e
optNeutral (Mult (Const 1) e) = Just e
optNeutral (Mult e (Const 1)) = Just e
optNeutral (Div e (Const 1)) = Just e
optNeutral _ = Nothing

-- Absorbing element optimization
optAbsorbing :: Exp -> Maybe Exp
optAbsorbing (Mult (Const 0) _) = Just (Const 0)
optAbsorbing (Mult _ (Const 0)) = Just (Const 0)
optAbsorbing (Div (Const 0) _) = Just (Const 0)
optAbsorbing _ = Nothing

-- Negatibe operations optimization
optNegative :: Exp -> Maybe Exp
optNegative (Neg (Neg e)) = Just e
optNegative (Neg (Const a)) = Just (Const (-a))
optNegative _ = Nothing

-- Basic operations optimization
optOperation :: Exp -> Maybe Exp
optOperation (Add (Const a) (Const b)) = Just (Const (a+b))
optOperation (Add e1 e2) = Just (Add (opt e1) (opt e2))
optOperation (Sub e1 e2) = Just (Add (opt e1) (Neg (opt e2)))
optOperation (Mult e1 e2) = Just (Mult (opt e1) (opt e2))
optOperation (Div e1 e2) = Just (Div (opt e1) (opt e2))
optOperation _ = Nothing

-- Comparison operations optimization
optEq :: Exp -> Maybe Exp
optEq (Equal (Const a) (Const b)) = Just (Bool (a == b))
optEq (Greater (Const a) (Const b)) = Just (Bool (a > b))
optEq (Less (Const a) (Const b)) = Just (Bool (a < b))
optEq (GreaterEqual (Const a) (Const b)) = Just (Bool (a >= b))
optEq (LessEqual (Const a) (Const b)) = Just (Bool (a <= b))
optEq _ = Nothing

-- Loop optimization
optLoop :: Inst -> Maybe Inst
optLoop (While (Const 0) b) = Just (While (Boolean False) b)
optLoop (While (Const _) b) = Just (While (Boolean True) b)
optLoop _ = Nothing

-- Conditionals optimization
optCond :: Inst -> Maybe Inst
optCond (ITE (Not cond) b1 b2) = Just (ITE cond b2 b1)
optCond _ = Nothing 