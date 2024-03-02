module Unparser where

import PicoC
import Control.Concurrent (Chan)
--------------------------------------------------------
-- Unparser : Da AST para texto (pretty printer)
--------------------------------------------------------

--instance Show PicoC where
--    show = Unparser

--unparser :: PicoC -> String
--unparser = upExp1


-- (Add (Const 3) (Div (Const 5) (Const 5)))
-- (Div (Add (Const 3) (Const 5)) (Const 5))
-- (GreaterEqual (Add (Const 3) (Const 5)) (Sub (Const 2) (Var "aux")))

-- (PicoC [DeclAtrib Int "margem" (Const 0),While (Less (Const 2) (Const 3)) [Atrib "margem" (Add (Mult (Const 4) (Const 23)) (Const 3))]]

upPicoC :: PicoC -> String
upPicoC (PicoC b) = upBlocoC b

upBlocoC :: BlocoC -> String
upBlocoC [] = ""
upBlocoC [h] = upInst h
upBlocoC (h:t) = upInst h ++ " " ++ upBlocoC t


upInst :: Inst -> String
upInst (Atrib s e) = s ++ " = " ++ upExp1 e ++ ";"                        
upInst (DeclAtrib t s e) = unType t ++ " " ++ s ++ " = " ++ upExp1 e ++ ";"
upInst (Decl t s) = unType t ++ " " ++ s ++ ";"
upWhile (While exp b) = "while( " ++ upExpEq exp ++ "){" ++ upBlocoC b ++ "}"
upITE (ITE exp b1 b2) = "if( " ++ upExpEq exp ++ ") then{" ++ upBlocoC b1 ++ "}else{" ++ upBlocoC b2 ++ "}"


upType :: Type -> String
upType Int = "int"
upType Char = "char"
upType String = "string"
upType Bool = "bool"
upType Void = "void"

upExpEq :: Exp -> String
upExpEq (Equal a b) = upExp1 a ++ " = " ++ upExp1 b
upExpEq (Greater a b) = upExp1 a ++ " > " ++ upExp1 b
upExpEq (Less a b) = upExp1 a ++ " < " ++ upExp1 b
upExpEq (GreaterEqual a b) = upExp1 a ++ " >= " ++ upExp1 b
upExpEq (LessEqual a b) = upExp1 a ++ " <= " ++ upExp1 b
upExpEq e = upExp1 e

upExp1 :: Exp -> String
upExp1 (Add a b) = upExp1 a ++ " + " ++ upExp1 b
upExp1 (Sub a b) = upExp1 a ++ " - " ++ upExp1 b
upExp1 e = upExp0 e

upExp0 :: Exp -> String
upExp0 (Mult a b) = upFactor a ++ " * " ++ upExp0 b
upExp0 (Div a b) = upFactor a ++ " / " ++ upExp0 b
upExp0 e = upFactor e

upFactor :: Exp -> String
upFactor (Const a) = show a
upFactor (Var a) = a
upFactor e = "(" ++ upExp1 e ++ ")"

