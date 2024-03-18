module Opt where

import PicoC

import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Data.Maybe (Maybe(Nothing))

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

-- Main function TopDown
optTD :: PicoC -> PicoC
optTD program = 
        let pProgram = toZipper program
            (Just newProgram) = applyTP (full_tdTP step) pProgram
            step = idTP `adhocTP` optOperationsTD `adhocTP` optInstTD
        in fromZipper newProgram

-- Exp optimization
optOperationsTD :: Exp -> Maybe Exp
optOperationsTD (Add (Const 0) e) = Just e
optOperationsTD (Add e (Const 0)) = Just e
optOperationsTD (Mult (Const 1) e) = Just e
optOperationsTD (Mult e (Const 1)) = Just e
optOperationsTD (Div e (Const 1)) = Just e
optOperationsTD (Mult (Const 0) _) = Just (Const 0)
optOperationsTD (Mult _ (Const 0)) = Just (Const 0)
optOperationsTD (Div (Const 0) _) = Just (Const 0)
optOperationsTD (Neg (Neg e)) = Just e
optOperationsTD (Neg (Const a)) = Just (Const (-a))
optOperationsTD (Add (Const a) (Const b)) = Just (Const (a+b))
optOperationsTD e = Just e

-- Inst optimization
optInstTD :: Inst -> Maybe Inst
optInstTD (While (Const 0) b) = Just (While (Boolean False) b)
optInstTD (While (Const _) b) = Just (While (Boolean True) b)
optInstTD (For r1 (Const 0) r3 r4) = Just (For r1 (Boolean False) r3 r4)
optInstTD (For r1 (Const _) r3 r4) = Just (For r1 (Boolean True) r3 r4)
optInstTD (For [] r2 [] r4) = Just (While r2 r4)
optInstTD e = Just e




-- Main function Innermost
optIM :: PicoC -> PicoC
optIM program = 
        let pProgram = toZipper program
            (Just newProgram) = applyTP (innermost step) pProgram
            step = failTP `adhocTP` optOperationsIM `adhocTP` optInstIM
        in fromZipper newProgram

-- Exp optimization
optOperationsIM :: Exp -> Maybe Exp
optOperationsIM (Add (Const 0) e) = Just e
optOperationsIM (Add e (Const 0)) = Just e
optOperationsIM (Mult (Const 1) e) = Just e
optOperationsIM (Mult e (Const 1)) = Just e
optOperationsIM (Div e (Const 1)) = Just e
optOperationsIM (Mult (Const 0) _) = Just (Const 0)
optOperationsIM (Mult _ (Const 0)) = Just (Const 0)
optOperationsIM (Div (Const 0) _) = Just (Const 0)
optOperationsIM (Neg (Neg e)) = Just e
optOperationsIM (Neg (Const a)) = Just (Const (-a))
optOperationsIM (Add (Const a) (Const b)) = Just (Const (a+b))
optOperationsIM _ = Nothing

-- Inst optimization
optInstIM :: Inst -> Maybe Inst
optInstIM (While (Const 0) b) = Just (While (Boolean False) b)
optInstIM (While (Const _) b) = Just (While (Boolean True) b)
optInstIM (For r1 (Const 0) r3 r4) = Just (For r1 (Boolean False) r3 r4)
optInstIM (For r1 (Const _) r3 r4) = Just (For r1 (Boolean True) r3 r4)
optInstIM (For [] r2 [] r4) = Just (While r2 r4)
optInstIM _ = Nothing



-- Refactoring
refactor :: PicoC -> PicoC
refactor program = 
                    let pProgram = toZipper program
                        (Just newProgram) = applyTP (innermost step) pProgram
                        step = failTP `adhocTP` ifRefactor
                    in fromZipper newProgram

ifRefactor :: Inst -> Maybe Inst
ifRefactor (ITE (Not cond) b1 b2) = Just (ITE cond b2 b1)                                   -- if (!cond) b1; else b2;                       -> if (cond) b2; else b1;
ifRefactor (ITE func [Return (Boolean True)] [Return (Boolean False)]) = Just (Return func) -- if ( func() ) return true; else return false; -> return func();
ifRefactor _ = Nothing
