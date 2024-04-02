{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
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
-- Optimization Functions
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
optOperationsTD (Add (Const a) (Const b)) = Just (Const (a+b))
optOperationsTD (Mult (Const 1) e) = Just e
optOperationsTD (Mult e (Const 1)) = Just e
optOperationsTD (Mult (Const 0) _) = Just (Const 0)
optOperationsTD (Mult _ (Const 0)) = Just (Const 0)
optOperationsTD (Div e (Const 1)) = Just e
optOperationsTD (Div (Const 0) _) = Just (Const 0)
optOperationsTD (Neg (Neg e)) = Just e
optOperationsTD (Neg (Const a)) = Just (Const (-a))
optOperationsTD (Or (Const 0) exp) = Just exp                                     -- (0 || a) -> a
optOperationsTD (Or exp (Const 0)) = Just exp                                     -- (a || 0) -> a  
optOperationsTD (Or (Const _) exp) = Just (Const 1)                               -- (1 || a) -> true
optOperationsTD (Or exp (Const _)) = Just (Const 1)                               -- (a || 1) -> true
optOperationsTD (And (Const 0) exp) = Just (Const 0)                              -- (0 && a) -> false
optOperationsTD (And exp (Const 0)) = Just (Const 0)                              -- (a && 0) -> false
optOperationsTD (And (Const _) exp) = Just exp                                    -- (1 && a) -> a
optOperationsTD (And exp (Const _)) = Just exp                                    -- (a && 1) -> a
optOperationsTD (Not (Not exp)) = Just exp                                        -- (!(!a)) -> a
optOperationsTD (Not (Const 0)) = Just (Const 1)                                  -- (!0) -> true;
optOperationsTD (Not (Const _)) = Just (Const 0)                                  -- (!1) -> false;
optOperationsTD (Equal (Const a) (Const b)) = Just (Boolean (a == b))             -- (a == b) -> true/false
optOperationsTD (Greater (Const a) (Const b)) = Just (Boolean (a > b))            -- (a > b) -> true/false
optOperationsTD (Less (Const a) (Const b)) = Just (Boolean (a < b))               -- (a < b) -> true/false
optOperationsTD (GreaterEqual (Const a) (Const b)) = Just (Boolean (a >= b))      -- (a >= b) -> true/false
optOperationsTD (LessEqual (Const a) (Const b)) = Just (Boolean (a <= b))         -- (a <= b) -> true/false
optOperationsTD (Equal (Boolean a) (Boolean b)) = Just (Boolean (a == b))         -- (a == b) -> true/false
optOperationsTD e = Just e

-- Inst optimization
optInstTD :: Inst -> Maybe Inst
optInstTD (While (Const 0) b) = Just (While (Boolean False) b)             -- while (0) b -> while (false) b
optInstTD (While (Const _) b) = Just (While (Boolean True) b)              -- while (1) b -> while (true) b
optInstTD (For r1 (Const 0) r3 r4) = Just (For r1 (Boolean False) r3 r4)   -- for (_; 0; _) b -> for (_; false; _) b
optInstTD (For r1 (Const _) r3 r4) = Just (For r1 (Boolean True) r3 r4)    -- for (_; 1; _) b -> for (_; true; _) b
optInstTD (For [] r2 [] r4) = Just (While r2 r4)                           -- for (; r2; ) b -> while (r2) b
optInstTD (For [] r2 r3 r4) = Just (While r2 (r4 ++ r3))                   -- for (; r2; r3) b -> while (r2) r4; r3;     
optInstTD e = Just e


--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------


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
optOperationsIM (Add (Const a) (Const b)) = Just (Const (a+b))
optOperationsIM (Mult (Const 1) e) = Just e
optOperationsIM (Mult e (Const 1)) = Just e
optOperationsIM (Mult (Const 0) _) = Just (Const 0)
optOperationsIM (Mult _ (Const 0)) = Just (Const 0)
optOperationsIM (Div e (Const 1)) = Just e
optOperationsIM (Div (Const 0) _) = Just (Const 0)
optOperationsIM (Neg (Neg e)) = Just e
optOperationsIM (Neg (Const a)) = Just (Const (-a))
optOperationsIM (Or (Const 0) exp) = Just exp                
optOperationsIM (Or exp (Const 0)) = Just exp                  
optOperationsIM (Or (Const _) exp) = Just (Const 1)         
optOperationsIM (Or exp (Const _)) = Just (Const 1)          
optOperationsIM (And (Const 0) exp) = Just (Const 0)         
optOperationsIM (And exp (Const 0)) = Just (Const 0)        
optOperationsIM (And (Const _) exp) = Just exp               
optOperationsIM (And exp (Const _)) = Just exp               
optOperationsIM (Not (Not exp)) = Just exp                   
optOperationsIM (Not (Const 0)) = Just (Const 1)             
optOperationsIM (Not (Const _)) = Just (Const 0)  
optOperationsIM (Equal (Const a) (Const b)) = Just (Boolean (a == b))            
optOperationsIM (Greater (Const a) (Const b)) = Just (Boolean (a > b))           
optOperationsIM (Less (Const a) (Const b)) = Just (Boolean (a < b))              
optOperationsIM (GreaterEqual (Const a) (Const b)) = Just (Boolean (a >= b))     
optOperationsIM (LessEqual (Const a) (Const b)) = Just (Boolean (a <= b))
optOperationsIM (Equal (Boolean a) (Boolean b)) = Just (Boolean (a == b))
optOperationsIM _ = Nothing

-- Inst optimization
optInstIM :: Inst -> Maybe Inst
optInstIM (While (Const 0) b) = Just (While (Boolean False) b)
optInstIM (While (Const _) b) = Just (While (Boolean True) b)
optInstIM (For r1 (Const 0) r3 r4) = Just (For r1 (Boolean False) r3 r4)
optInstIM (For r1 (Const _) r3 r4) = Just (For r1 (Boolean True) r3 r4)
optInstIM (For [] r2 [] r4) = Just (While r2 r4)
optInstIM (For [] r2 r3 r4) = Just (While r2 (r4 ++ r3))                        
optInstIM _ = Nothing



--------------------------------------------------------
-- Refactoring Functions
--------------------------------------------------------

-- Main function Innermost
refactor :: PicoC -> PicoC
refactor program = 
                    let pProgram = toZipper program
                        (Just newProgram) = applyTP (innermost step) pProgram
                        step = failTP `adhocTP` ifRefactor
                    in fromZipper newProgram

ifRefactor :: Inst -> Maybe Inst
ifRefactor (ITE (Not cond) b1 b2) = Just (ITE cond b2 b1)                                   -- if (!cond) b1; else b2;                     -> if (cond) b2; else b1;
ifRefactor (ITE exp [Return (Boolean True)] [Return (Boolean False)]) = Just (Return exp)   -- if ( exp ) return true; else return false;  -> return exp;
ifRefactor _ = Nothing
