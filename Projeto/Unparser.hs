module Unparser where

import PicoC
import Control.Concurrent (Chan)

--------------------------------------------------------
-- Unparser : Da AST para texto (pretty printer)
--------------------------------------------------------

--instance Show PicoC where
--    show = Unparser

upPicoC :: PicoC -> String
upPicoC (PicoC b) = upBlocoC b

upBlocoC :: BlocoC -> String
upBlocoC [] = ""
upBlocoC [h] = upInst h
upBlocoC (h:t) = upInst h ++ " " ++ upBlocoC t


upInst :: Inst -> String
upInst (Atrib s e) = s ++ " = " ++ upExp1 e ++ ";"
upInst (DeclAtrib t s e) = upType t ++ " " ++ s ++ " = " ++ upExp1 e ++ ";"
upInst (Decl t s) = upType t ++ " " ++ s ++ ";"
upInst (While exp b) = "while (" ++ upExpLogicos exp ++ ") { " ++ upBlocoC b ++ " }"
upInst (ITE exp b1 b2) = "if (" ++ upExpLogicos exp ++ ") then { " ++ upBlocoC b1 ++ " } else { " ++ upBlocoC b2 ++ " }"


upType :: Type -> String
upType Int = "int"
upType Char = "char"
upType String = "string"
upType Bool = "bool"
upType Void = "void"

upExpLogicos :: Exp -> String
upExpLogicos (And a b) = upExpEq a ++ " && " ++ upExpLogicos b
upExpLogicos (Or a b) = upExpEq a ++ " || " ++ upExpLogicos b
upExpLogicos e = upExpEq e

upExpEq :: Exp -> String
upExpEq (Equal a b) = upExp1 a ++ " == " ++ upExpEq b
upExpEq (Greater a b) = upExp1 a ++ " > " ++ upExpEq b
upExpEq (Less a b) = upExp1 a ++ " < " ++ upExpEq b
upExpEq (GreaterEqual a b) = upExp1 a ++ " >= " ++ upExpEq b
upExpEq (LessEqual a b) = upExp1 a ++ " <= " ++ upExpEq b
upExpEq e = upExp1 e

upExp1 :: Exp -> String
upExp1 (Add a b) = upExp0 a ++ " + " ++ upExp1 b
upExp1 (Sub a b) = upExp0 a ++ " - " ++ upExp1 b
upExp1 e = upExp0 e

upExp0 :: Exp -> String
upExp0 (Mult a b) = upFactor a ++ " * " ++ upExp0 b
upExp0 (Div a b) = upFactor a ++ " / " ++ upExp0 b
upExp0 e = upFactor e

upFactor :: Exp -> String
upFactor (Const a) = show a
upFactor (Var a) = a
upFactor (Boolean a) = show a
upFactor e = "(" ++ upExp1 e ++ ")"

