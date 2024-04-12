{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module TestGenerator where

import Test.QuickCheck

import PicoC
import Prop

instance Arbitrary PicoC where
  arbitrary = genPicoC 1 1 2   -- genPicoC numFuncs numInsts maxExps

-- Type Generator
genType :: Gen Type
genType = elements [Int, Char, String, Bool, Void]

genInt :: Gen Int
genInt = choose (0, 100)

genChar :: Gen Char
genChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

genString :: Gen String
genString = do
              numChars <- choose (1, 5)
              vectorOf numChars genChar

genBool :: Gen Bool
genBool = choose (False, True)



-- Arg Generator
genArgs :: Gen [Arg]
genArgs = do
            numArgs <- choose (0,5)
            vectorOf numArgs genArg

genArg :: Gen Arg
genArg = do
            tp <- suchThat genType (/= Void)
            h <- elements ['a'..'z']            -- Must be a valid name (starts with a lowercase letter)
            t <- genString
            return (Arg tp (h:t))



-- ArgCall Generator
genArgCalls :: Gen [ArgCall]
genArgCalls = do
                numArgs <- choose (0,5)
                vectorOf numArgs genArgCall

genArgCall :: Gen ArgCall
genArgCall = do
                h <- elements ['a'..'z']
                t <- genString
                return (ArgCall (h:t))



-- Inst Generator
genInsts :: Int -> Int -> Gen [Inst]
genInsts maxInsts maxExps = do
                                numInsts <- choose (1, maxInsts)
                                numExps <- choose (1, maxExps)
                                vectorOf numInsts (genInst numInsts numExps)

genInst :: Int -> Int -> Gen Inst
genInst numInsts numExps = oneof [genDecl, genDeclAtrib numExps, genAtrib numExps,
                                  genDeclAtribFuncCall, genAtribFuncCall, genWhile numInsts numExps,
                                  genFor numInsts numExps, genITE numInsts numExps,
                                  genCallFunc, genReturn numExps]

genDecl :: Gen Inst
genDecl = do
            tp <- suchThat genType (/= Void)
            h <- elements ['a'..'z']
            t <- genString
            return (Decl tp (h:t))

genDeclAtrib :: Int -> Gen Inst
genDeclAtrib numExps = do
                        tp <- suchThat genType (/= Void)
                        h <- elements ['a'..'z']
                        t <- genString
                        exp <- genOps numExps
                        return (DeclAtrib tp (h:t) exp)

genAtrib :: Int -> Gen Inst
genAtrib numExps = do
                    h <- elements ['a'..'z']
                    t <- genString
                    exp <- genOps numExps
                    return (Atrib (h:t) exp)


genDeclAtribFuncCall :: Gen Inst
genDeclAtribFuncCall = do
                    tp <- suchThat genType (/= Void)
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

genCallFunc :: Gen Inst
genCallFunc = do
                h <- elements ['a'..'z']
                t <- genString
                args <- genArgCalls
                return (CallFunc (h:t) args)

genWhile :: Int -> Int -> Gen Inst
genWhile numInsts numExps = do
                                exp <- genExp numExps
                                insts <- genBlocoC numInsts numExps
                                return (While exp insts)


genFor :: Int -> Int -> Gen Inst
genFor numInsts numExps = do
                            init <- oneof [genDeclAtrib numExps, genAtrib numExps]    -- just ints or char
                            exp <- genExp numExps
                            inc <- genAtrib numExps                                   -- cannot be aux = True; junt ints or char
                            insts <- genBlocoC numInsts numExps
                            return (For [init] exp [inc] insts)

genITE :: Int -> Int -> Gen Inst
genITE numInsts numExps = do
                            exp <- genExp numExps
                            t <- genBlocoC numInsts numExps
                            e <- genBlocoC numInsts numExps
                            return (ITE exp t e)

genReturn :: Int -> Gen Inst
genReturn numExps = do
                        exp <- genExp numExps
                        return (Return exp)



-- BlocoC Generator
genBlocoC :: Int -> Int -> Gen BlocoC
genBlocoC numInsts numExps = vectorOf numInsts (genInst numInsts numExps)



-- Exp Generator
instance Arbitrary Exp where
 arbitrary = sized genExp
 shrink = shrinkExp

------------
-- genExp --
------------
genExp :: Int -> Gen Exp
genExp 1 = genOps 1
genExp n = oneof [genOps n, genGreater n, genLess n, genEqual n, genGreaterEqual n,
                  genLessEqual n, genAnd n, genOr n, genNot n]

genOps :: Int -> Gen Exp
genOps 1 = oneof [genConst, genNeg, genVar, genBoolean]
genOps n = oneof [genAdd n, genSub n, genMult n, genDiv n]



genAdd :: Int -> Gen Exp
genAdd n = do
            e1 <- if even n then genOps (div n 2)
                            else genOps (div n 2 + 1)
            e2 <- genOps (div n 2)
            return (Add e1 e2)

genSub :: Int -> Gen Exp
genSub n = do
            e1 <- if even n then genOps (div n 2)
                            else genOps (div n 2 + 1)
            e2 <- genOps (div n 2)
            return (Sub e1 e2)

genMult :: Int -> Gen Exp
genMult n = do
                e1 <- if even n then genOps (div n 2)
                                else genOps (div n 2 + 1)
                e2 <- genOps (div n 2)
                return (Mult e1 e2)

genDiv :: Int -> Gen Exp
genDiv n = do
            e1 <- if even n then genOps (div n 2)
                            else genOps (div n 2 + 1)
            e2 <- genOps (div n 2)
            return (Div e1 e2)

genConst :: Gen Exp
genConst = do
            x <- genInt         -- arbitrary means that the value x can be any value of the type
            return (Const x)

genNeg :: Gen Exp
genNeg = do
            x <- genInt
            return (Neg (Const x))

genVar :: Gen Exp
genVar = do
            h <- elements ['a'..'z']
            t <- genString
            return (Var (h:t))

genBoolean :: Gen Exp
genBoolean = Boolean <$> genBool

genGreater :: Int -> Gen Exp
genGreater n = do
                e1 <- if even n then genExp (div n 2)
                                else genExp (div n 2 + 1)
                e2 <- genExp (div n 2)
                return (Greater e1 e2)

genLess :: Int -> Gen Exp
genLess n = do
                e1 <- if even n then genExp (div n 2)
                                else genExp (div n 2 + 1)
                e2 <- genExp (div n 2)
                return (Less e1 e2)

genEqual :: Int -> Gen Exp
genEqual n = do
                e1 <- if even n then genExp (div n 2)
                                else genExp (div n 2 + 1)
                e2 <- genExp (div n 2)
                return (Equal e1 e2)

genGreaterEqual :: Int -> Gen Exp
genGreaterEqual n = do
                        e1 <- if even n then genExp (div n 2)
                                        else genExp (div n 2 + 1)
                        e2 <- genExp (div n 2)
                        return (GreaterEqual e1 e2)

genLessEqual :: Int -> Gen Exp
genLessEqual n = do
                    e1 <- if even n then genExp (div n 2)
                                    else genExp (div n 2 + 1)
                    e2 <- genExp (div n 2)
                    return (LessEqual e1 e2)

genAnd:: Int -> Gen Exp
genAnd n = do
            e1 <- if even n then genExp (div n 2)
                            else genExp (div n 2 + 1)
            e2 <- genExp (div n 2)
            return (And e1 e2)

genOr:: Int -> Gen Exp
genOr n = do
            e1 <- if even n then genExp (div n 2)
                            else genExp (div n 2 + 1)
            e2 <- genExp (div n 2)
            return (Or e1 e2)

genNot :: Int -> Gen Exp
genNot n = do
            e1 <- genExp n
            return (Not e1)

------------
-- Shrink --
------------
shrinkExp :: Exp -> [Exp]
shrinkExp (Add e1 e2)  = [e1, e2] ++ [Add e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Sub e1 e2)  = [e1, e2] ++ [Sub e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Mult e1 e2) = [e1, e2] ++ [Mult e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Div e1 e2)  = [e1, e2] ++ [Div e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Neg e) = e : [Neg e' | e' <- shrinkExp e]
shrinkExp (Const i) = [Const i' | i' <- shrink i]
shrinkExp (Var v)   = [Var v' | v' <- shrink v]
shrinkExp (Boolean b) = [Boolean b' | b' <- shrink b]
shrinkExp (Greater e1 e2) = [e1, e2] ++ [Greater e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Less e1 e2) = [e1, e2] ++ [Less e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Equal e1 e2) = [e1, e2] ++ [Equal e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (GreaterEqual e1 e2) = [e1, e2] ++ [GreaterEqual e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (LessEqual e1 e2) = [e1, e2] ++ [LessEqual e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (And e1 e2) = [e1, e2] ++ [And e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Or e1 e2) = [e1, e2] ++ [Or e1' e2' | e1' <- shrinkExp e1, e2' <- shrinkExp e2]
shrinkExp (Not e) = e : [Not e' | e' <- shrinkExp e]



-- Func Generator
genFunc :: Int -> Int -> Gen Func
genFunc maxInsts maxExps = do
                            fType <- genType
                            h <- elements ['a'..'z']
                            t <- genString
                            args <- genArgs
                            insts <- genInsts maxInsts maxExps
                            return (Func fType (h:t) args insts)



-- PicoC Generator
genPicoC :: Int -> Int -> Int -> Gen PicoC
genPicoC maxFuncs maxInsts maxExps = do
                                        numFuncs <- choose (1, maxFuncs)
                                        funcs <- vectorOf numFuncs (genFunc maxInsts maxExps)
                                        return (PicoC funcs)