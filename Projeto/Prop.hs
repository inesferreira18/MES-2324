{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Prop where

import PicoC
import Unparser
import Data.Fixed (Pico)
import Opt
import Data.Maybe

-----------------
-- Deriving Eq --
-----------------

instance Eq PicoC where
  (PicoC code1) == (PicoC code2) = code1 == code2
  _ == _ = False

instance Eq Func where
  (Func t1 s1 a1 i1) == (Func t2 s2 a2 i2) = t1 == t2 && s1 == s2 && a1 == a2 && i1 == i2
  _ == _ = False

instance Eq Arg where
  (Arg t1 s1) == (Arg t2 s2) = t1 == t2 && s1 == s2
  _ == _ = False

instance Eq ArgCall where
  (ArgCall s1) == (ArgCall s2) = s1 == s2
  _ == _ = False

instance Eq Inst where
  (DeclAtrib t1 a1 exp1) == (DeclAtrib t2 a2 exp2) = t1 == t2 && a1 == a2 && exp1 == exp2
  (Decl t1 a1) == (Decl t2 a2) = t1 == t2 && a1 == a2
  (Atrib a1 exp1) == (Atrib a2 exp2) = a1 == a2 && exp1 == exp2
  (DeclAtribFuncCall t1 a1 i1) == (DeclAtribFuncCall t2 a2 i2) = t1 == t2 && a1 == a2 && i1 == i2
  (AtribFuncCall a1 i1) == (AtribFuncCall a2 i2) = a1 == a2 && i1 == i2
  (ITE exp1 tb1 eb1) == (ITE exp2 tb2 eb2) = exp1 == exp2 && tb1 == tb2 && eb1 == eb2
  (While exp1 b1) == (While exp2 b2) = exp1 == exp2 && b1 == b2
  (For init1 cond1 inc1 b1) == (For init2 cond2 inc2 b2) = init1 == init2 && cond1 == cond2 && inc1 == inc2 && b1 == b2
  (Print s1) == (Print s2) = s1 == s2
  (CallFunc s1 a1) == (CallFunc s2 a2) = s1 == s2 && a1 == a2
  (Return exp1) == (Return exp2) = exp1 == exp2
  _ == _ = False

instance Eq Type where
  Int == Int = True
  Char == Char = True
  String == String = True
  Bool == Bool = True
  Void == Void = True
  _ == _ = False

instance Eq Exp where
  (Add e1 e2) == (Add e3 e4) = e1 == e3 && e2 == e4
  (Sub e1 e2) == (Sub e3 e4) = e1 == e3 && e2 == e4
  (Mult e1 e2) == (Mult e3 e4) = e1 == e3 && e2 == e4
  (Div e1 e2) == (Div e3 e4) = e1 == e3 && e2 == e4
  (Neg e1) == (Neg e2) = e1 == e2
  (Const c1) == (Const c2) = c1 == c2
  (Var v1) == (Var v2) = v1 == v2
  (Boolean b1) == (Boolean b2) = b1 == b2
  (Equal e1 e2) == (Equal e3 e4) = e1 == e3 && e2 == e4
  (Less e1 e2) == (Less e3 e4) = e1 == e3 && e2 == e4
  (Greater e1 e2) == (Greater e3 e4) = e1 == e3 && e2 == e4
  (LessEqual e1 e2) == (LessEqual e3 e4) = e1 == e3 && e2 == e4
  (GreaterEqual e1 e2) == (GreaterEqual e3 e4) = e1 == e3 && e2 == e4
  (And e1 e2) == (And e3 e4) = e1 == e3 && e2 == e4
  (Or e1 e2) == (Or e3 e4) = e1 == e3 && e2 == e4
  (Not e1) == (Not e2) = e1 == e2
  _ == _ = False



----------------------
-- Property Testing --
-- quickCheck prop_ --  
----------------------

-- Test if the unparser is the inverse of the parser
prop_PicoC :: PicoC -> Bool
prop_PicoC ast = ast == fst (last $ pPicoC (upPicoC ast))

-- Test if the  innermost strategy is idempotent
prop_Innermost :: PicoC -> Bool
prop_Innermost ast = optIM ast == optIM (optIM ast)

-- Test if different strategies are equivalent (innermost and topdown; should return False)
prop_Strategies :: PicoC -> Bool
prop_Strategies ast = optIM ast == optTD ast

-- Test the commutativity of the optimizations with the refactorings
prop_OptCommutativeRef :: PicoC -> Bool
prop_OptCommutativeRef ast = optIM (refactor (optIM ast)) == refactor (optIM ast)
