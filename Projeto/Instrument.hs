module Instrument where

import PicoC
import Eval

import Data.List (isInfixOf)

--------------------------------------------------------
-- Intrumentation Functions
--------------------------------------------------------

instrumentedTestSuite :: PicoC -> [([Int],Int)] -> Bool
instrumentedTestSuite p ts = runTestSuite (instrumentation p) ts

instrumentation :: PicoC -> PicoC
instrumentation (PicoC funcs) = PicoC (instrumentFuncs funcs)

instrumentFuncs :: [Func] -> [Func]
instrumentFuncs [] = []
instrumentFuncs (h:t) = (instrumentFunc h) : (instrumentFuncs t)

instrumentFunc :: Func -> Func
instrumentFunc (Func tp name args b) = Func tp name args (instrumentInsts b 2)

instrumentInsts :: [Inst] -> Int -> [Inst]
instrumentInsts [] _ = []
instrumentInsts (h:t) lineNumber = let (instr, nextLine) = instrumentInst h lineNumber
                                   in instr ++ instrumentInsts t nextLine 

-- Instrument an instruction by adding a print statement with the line number
instrumentInst :: Inst -> Int -> ([Inst], Int)
instrumentInst (Decl tp name) lineNumber =  ([Print ("Line: " ++ show lineNumber ++ " executed DECL")] ++ [Decl tp name], lineNumber+1)  
instrumentInst (DeclAtrib tp name exp) lineNumber = ([Print ("Line: " ++ show lineNumber ++ " executed DECL_ATRIB")] ++ [DeclAtrib tp name exp], lineNumber+1) 
instrumentInst (Atrib a exp) lineNumber = ([Print ("Line: " ++ show lineNumber ++ " executed ATRIB")] ++ [Atrib a exp], lineNumber+1)
instrumentInst (Print s) lineNumber = ([Print ("Line: " ++ show lineNumber ++ " executed PRINT")] ++ [Print s], lineNumber+1)
instrumentInst (DeclAtribFuncCall tp name func) lineNumber = ([Print ("Line: " ++ show lineNumber ++ " executed DECL_ATRIB_FUNC_CALL")] ++ [DeclAtribFuncCall tp name func], lineNumber+1)
instrumentInst (AtribFuncCall a func) lineNumber = ([Print ("Line: " ++ show lineNumber ++ " executed ATRIB_FUNC_CALL")] ++ [AtribFuncCall a func], lineNumber+1)
instrumentInst (CallFunc name args) lineNumber = ([Print ("Line: " ++ show lineNumber ++ " executed CALL_FUNC")] ++ [CallFunc name args], lineNumber+1)
instrumentInst (Return exp) lineNumber = ([Print ("Line: " ++ show lineNumber ++ " executed RETURN")] ++ [Return exp], lineNumber+1)
instrumentInst (While exp b) lineNumber = let (while_b, endline) = instrumentBlock b (lineNumber+1)
                                          in ([Print ("Line: " ++ show lineNumber ++ " executed WHILE LOOP")] ++ [While exp while_b], endline)
instrumentInst (For init exp inc b) lineNumber = let (for_b, endline) = instrumentBlock b (lineNumber+1)
                                                 in ([Print ("Line: " ++ show lineNumber ++ " executed FOR LOOP")] ++ [For init exp inc for_b], endline) 
instrumentInst (ITE exp b1 b2) lineNumber = let (then_b, then_line) = instrumentBlock b1 (lineNumber+1)
                                                (else_b, else_line) = instrumentBlock b2 then_line
                                            in ([Print ("Line: " ++ show lineNumber ++ " executed ITE BLOCK")] ++ [ITE exp then_b else_b], else_line)    

instrumentBlock :: [Inst] -> Int -> ([Inst], Int)
instrumentBlock [] lineNumber = ([], lineNumber)
instrumentBlock (h:t) lineNumber = let (instr, nextLine) = instrumentInst h lineNumber
                                       (instrs, finalLine) = instrumentBlock t nextLine
                                   in (instr ++ instrs, finalLine)

