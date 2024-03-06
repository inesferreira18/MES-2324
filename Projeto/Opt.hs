module Opt where

import PicoC

import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic

instance StrategicData PicoC
instance StrategicData Func
instance StrategicData Arg
instance StrategicData ArgCall
instance StrategicData Inst
instance StrategicData Type
instance StrategicData Exp
instance StrategicData a => StrategicData [a]

--------------------------------------------------------
-- Optimization Function
--------------------------------------------------------

-- Main function
opt :: PicoC -> PicoC
opt program = 
    let pProgram = toZipper program
        (Just newProgram) = applyTP (full_tdTP step) pProgram
        --step = idTP `adhocTP` optNeutral `adhocTP` optAbsorbing `adhocTP` optNegative `adhocTP` optOperation `adhocTP` optInst `adhocTP` optCond
        step = idTP `adhocTP` optOperations `adhocTP` optInst
    in fromZipper newProgram

-- Exp optimization
optOperations :: Exp -> Maybe Exp
optOperations (Add (Const 0) e) = Just e
optOperations (Add e (Const 0)) = Just e
optOperations (Mult (Const 1) e) = Just e
optOperations (Mult e (Const 1)) = Just e
optOperations (Div e (Const 1)) = Just e
optOperations (Mult (Const 0) _) = Just (Const 0)
optOperations (Mult _ (Const 0)) = Just (Const 0)
optOperations (Div (Const 0) _) = Just (Const 0)
optOperations (Neg (Neg e)) = Just e
optOperations (Neg (Const a)) = Just (Const (-a))
optOperations (Add (Const a) (Const b)) = Just (Const (a+b))
optOperations e = Just e

-- Inst optimization
optInst :: Inst -> Maybe Inst
optInst (While (Const 0) b) = Just (While (Boolean False) b)
optInst (While (Const _) b) = Just (While (Boolean True) b)
optInst (For r1 (Const 0) r3 r4) = Just (For r1 (Boolean False) r3 r4)
optInst (For r1 (Const _) r3 r4) = Just (For r1 (Boolean True) r3 r4)
optInst (For [] r2 [] r4) = Just (While r2 r4)
optInst (ITE (Not cond) b1 b2) = Just (ITE cond b2 b1)
optInst e = Just e
