{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module TestGenerator where

import Test.QuickCheck

import PicoC
import Data.ByteString (length, elem)
import Control.Monad (Monad(return))


-- Type Generator
genType :: Gen Type
genType = elements [Int, Char, String, Bool, Void]

genInt :: Gen Int
genInt = choose (0, 100)

genChar :: Gen Char
genChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])

genString :: Gen String
genString = do
              length <- choose(1, 10)
              vectorOf length genChar

genBool :: Gen Bool
genBool = choose (False, True)


-- Arg Generator
genArgs :: Gen [Arg]
genArgs = do
            length <- choose(0,5)
            vectorOf length genArg

genArg :: Gen Arg
genArg = do                          
            tp <- genType
            h <- elements ['a'..'z']    -- Must be a valid name
            t <- genString
            return (Arg tp (h:t))



-- ArgCall Generator
genArgCalls :: Gen [ArgCall]
genArgCalls = do
                length <- choose(0,5)
                vectorOf length genArgCall

genArgCall :: Gen ArgCall
genArgCall = do
                h <- elements ['a'..'z']
                t <- genString
                return (ArgCall (h:t))



-- Inst Generator
genInsts :: Int -> Gen [Inst]
genInsts numInsts = do
                      length <- choose(0, numInsts)
                      vectorOf length genInst

genInst :: Gen Inst
genInst = oneof [genDecl, genDeclAtrib, genAtrib, genDeclAtribFuncCall, genAtribFuncCall, genWhile, genFor, genITE, genCallFunc, genReturn]
    
genDecl :: Gen Inst
genDecl = do
            tp <- genType
            h <- elements ['a'..'z']
            t <- genString
            return (Decl tp (h:t))

genDeclAtrib :: Gen Inst
genDeclAtrib = do
                tp <- genType
                h <- elements ['a'..'z']
                t <- genString
                numExps <- choose(0, 1)
                exp <- genExp numExps
                return (DeclAtrib tp (h:t) exp)

genAtrib :: Gen Inst
genAtrib = do
                h <- elements ['a'..'z']
                t <- genString
                numExps <- choose(0, 1)
                exp <- genExp numExps
                return (Atrib (h:t) exp)


genDeclAtribFuncCall :: Gen Inst
genDeclAtribFuncCall = do
                    tp <- genType
                    h <- elements ['a'..'z']
                    t <- genString
                    inst <- genCallFunc
                    return (DeclAtribFuncCall tp (h:t) inst)

genAtribFuncCall :: Gen Inst
genAtribFuncCall = do
                    h <- elements ['a'..'z']
                    t <- genString
                    inst <- genCallFunc
                    return (AtribFuncCall (h:t) inst)

genWhile :: Gen Inst
genWhile = do
            numExps <- choose(0, 1)
            exp <- genExp numExps
            insts <- genInsts 1
            return (While exp insts)


genFor :: Gen Inst
genFor = do
            init <- oneof [genDeclAtrib, genAtrib]
            numExps <- choose(0, 1)
            exp <- genExp numExps
            inc <- genAtrib
            insts <- genInsts 1
            return (For [init] exp [inc] insts)

genITE :: Gen Inst
genITE = do 
            numExps <- choose(0, 1)
            exp <- genExp numExps
            t <- genInsts 1
            e <- genInsts 1
            return (ITE exp t e)

genCallFunc :: Gen Inst
genCallFunc = do
                h <- elements ['a'..'z']
                t <- genString
                args <- genArgCalls
                return (CallFunc (h:t) args)

genReturn :: Gen Inst
genReturn = do
                numExps <- choose(0, 1)
                exp <- genExp numExps
                return (Return exp)




-- Exp Generator
instance Arbitrary Exp where          
 arbitrary = sized genExp

genExp :: Int -> Gen Exp
genExp 0 = genOps 0
genExp n = oneof [genOps n, genBoolean, genGreater n, genLess n, genEqual n, genGreaterEqual n, genLessEqual n, genAnd n, genOr n, genNot n]

genOps :: Int -> Gen Exp
genOps 0 = genConst
genOps n = oneof [genAdd n, genSub n, genMult n, genDiv n, genVar]



genAdd :: Int -> Gen Exp
genAdd n = do
            e1 <- genOps (div n 2)
            e2 <- genOps (div n 2)
            return (Add e1 e2)

genSub :: Int -> Gen Exp
genSub n = do
            e1 <- genOps (div n 2)
            e2 <- genOps (div n 2)
            return (Sub e1 e2)

genMult :: Int -> Gen Exp
genMult n = do
            e1 <- genOps (div n 2)
            e2 <- genOps (div n 2)
            return (Mult e1 e2)

genDiv :: Int -> Gen Exp
genDiv n = do
            e1 <- genOps (div n 2)
            e2 <- genOps (div n 2)
            return (Div e1 e2)

--genNeg :: Int -> Gen Exp
--genNeg n = do
--            e1 <- genOps (div n 2)
--            return (Neg e1)

genConst :: Gen Exp
genConst = do
            x <- arbitrary    -- arbitrary means that the value x can be any value of the type
            return (Const x)

genVar :: Gen Exp
genVar = do
            h <- elements ['a'..'z']
            t <- genString
            return (Var (h:t))

genBoolean :: Gen Exp
genBoolean = Boolean <$> genBool

genGreater :: Int -> Gen Exp
genGreater n = do
                e1 <- genExp (div n 2)
                e2 <- genExp (div n 2)
                return (Greater e1 e2)

genLess :: Int -> Gen Exp
genLess n = do
                e1 <- genExp (div n 2)
                e2 <- genExp (div n 2)
                return (Less e1 e2)

genEqual :: Int -> Gen Exp
genEqual n = do
                e1 <- genExp (div n 2)
                e2 <- genExp (div n 2)
                return (Equal e1 e2)

genGreaterEqual :: Int -> Gen Exp
genGreaterEqual n = do
                e1 <- genExp (div n 2)
                e2 <- genExp (div n 2)
                return (GreaterEqual e1 e2)

genLessEqual :: Int -> Gen Exp
genLessEqual n = do
                e1 <- genExp (div n 2)
                e2 <- genExp (div n 2)
                return (LessEqual e1 e2)

genAnd:: Int -> Gen Exp 
genAnd n = do
            e1 <- genExp (div n 2)
            e2 <- genExp (div n 2)
            return (And e1 e2)

genOr:: Int -> Gen Exp 
genOr n = do
            e1 <- genExp (div n 2)
            e2 <- genExp (div n 2)
            return (Or e1 e2)
            
genNot :: Int -> Gen Exp 
genNot n = do
            e1 <- genExp (div n 2)
            return (Not e1)



-- Func Generator
genFunc :: Gen Func
genFunc = do
            tp <- genType
            h <- elements ['a'..'z'] 
            t <- genString
            args <- genArgs
            insts <- genInsts
            return (Func tp (h:t) args insts)

genPicoC :: Int -> Int -> Int -> Gen PicoC
genPicoC maxFuncs maxInsts maxExps = do
                                        numFuncs <- choose(1, maxFuncs)
                                        funcs <- vectorOf numFuncs (genFunc maxInsts maxExps)
                                        return (PicoC funcs)