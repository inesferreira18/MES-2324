{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module MutationGenerator where

import PicoC
import Test.QuickCheck
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Prop


--------------------------------------------------------
-- Mutation Generation
--------------------------------------------------------

-- Mutate a random Exp from the program
mutate :: PicoC -> Gen PicoC
mutate program = do
                    exp <- chooseExp program
                    mutation <- chooseMutation exp
                    let pProgram = toZipper program
                        (Just newProgram) = applyTP (full_tdTP step) pProgram
                        step = idTP `adhocTP` applyMutation exp mutation
                    return (fromZipper newProgram)

-- Apply a mutation to an Exp
applyMutation :: Exp -> Exp -> Exp -> Maybe Exp
applyMutation exp mutation e = if exp == e 
                               then Just mutation 
                               else Just e

-- Choose a random Exp from the program
chooseExp :: PicoC -> Gen Exp
chooseExp program = elements (expList program)

-- List of Exp from the program
expList :: PicoC -> [Exp]
expList (PicoC funcs) = concatMap expListFunc funcs

expListFunc :: Func -> [Exp]
expListFunc (Func _ _ _ insts) = concatMap expListInst insts

expListInst :: Inst -> [Exp]
expListInst (Decl _ _) = []
expListInst (DeclAtrib _ _ e) = [e]
expListInst (Atrib _ e) = [e]
expListInst (DeclAtribFuncCall _ _ inst) = expListInst inst
expListInst (AtribFuncCall _ inst) = expListInst inst
expListInst (While e bloco) = e : concatMap expListInst bloco
expListInst (For insts1 e insts2 bloco) = concatMap expListInst insts1 ++ [e] ++ concatMap expListInst bloco
expListInst (ITE e bloco1 bloco2) = e : concatMap expListInst bloco1 ++ concatMap expListInst bloco2
expListInst (CallFunc _ _) = []
expListInst (Return e) = [e]

-- Choose a mutation
chooseMutation :: Exp -> Gen Exp
chooseMutation (Add e1 e2) = chooseMutationOp e1 e2
chooseMutation (Sub e1 e2) = chooseMutationOp e1 e2
chooseMutation (Mult e1 e2) = chooseMutationOp e1 e2
chooseMutation (Div e1 e2) = chooseMutationOp e1 e2
chooseMutation (Greater e1 e2) = chooseMutationEq e1 e2
chooseMutation (Less e1 e2) = chooseMutationEq e1 e2
chooseMutation (Equal e1 e2) = chooseMutationEq e1 e2
chooseMutation (GreaterEqual e1 e2) = chooseMutationEq e1 e2
chooseMutation (LessEqual e1 e2) = chooseMutationEq e1 e2
chooseMutation (And e1 e2) = chooseMutationEq e1 e2
chooseMutation (Or e1 e2) = chooseMutationEq e1 e2
chooseMutation (Neg e) = return e
chooseMutation (Const i) = chooseMutationExpConst i
chooseMutation (Var n) = chooseMutationExpVar n
chooseMutation (Boolean b) = return (Boolean (not b))
chooseMutation e = return e

chooseMutationOp :: Exp -> Exp -> Gen Exp
chooseMutationOp e1 e2 = elements [Add e1 e2, Sub e1 e2, Mult e1 e2, Div e1 e2]

chooseMutationEq :: Exp -> Exp -> Gen Exp
chooseMutationEq e1 e2 = elements [Greater e1 e2, Less e1 e2, Equal e1 e2, GreaterEqual e1 e2, 
                                   LessEqual e1 e2, And e1 e2, Or e1 e2]

chooseMutationExpConst :: Int -> Gen Exp
chooseMutationExpConst i = do
                            num <- choose (-100,100)
                            return (Const num)

chooseMutationExpVar :: String -> Gen Exp
chooseMutationExpVar n = do 
                            numChars <- choose (1,5)
                            string <- vectorOf numChars (elements ['a'..'z'])
                            return (Var string)