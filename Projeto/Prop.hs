module Prop where

import PicoC
import Unparser
import Data.Fixed (Pico)

-----------------
-- Deriving Eq --
-----------------

instance Eq PicoC where
  (PicoC code1) == (PicoC code2) = code1 == code2
  _ == _ = False

instance Eq Inst where
  (DeclAtrib t1 a1 exp1) == (DeclAtrib t2 a2 exp2) = t1 == t2 && a1 == a2 && exp1 == exp2
  (Decl t1 a1) == (Decl t2 a2) = t1 == t2 && a1 == a2
  (Atrib a1 exp1) == (Atrib a2 exp2) = a1 == a2 && exp1 == exp2
  (ITE exp1 tb1 eb1) == (ITE exp2 tb2 eb2) = exp1 == exp2 && tb1 == tb2 && eb1 == eb2
  (While exp1 b1) == (While exp2 b2) = exp1 == exp2 && b1 == b2
  _ == _ = True

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
  _ == _ = False


----------------------
-- Property Testing --
----------------------

propPicoC :: PicoC -> Bool
propPicoC ast = ast == fst (last $ pPicoC (upPicoC ast))