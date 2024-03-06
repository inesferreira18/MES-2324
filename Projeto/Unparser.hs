module Unparser where

import PicoC
import Control.Concurrent (Chan)

--------------------------------------------------------
-- Unparser : Da AST para texto (pretty printer)
--------------------------------------------------------

--instance Show PicoC where
--    show = Unparser

upPicoC :: PicoC -> String
upPicoC (PicoC funcs) = upFuncs funcs 

upFuncs :: [Func] -> String
upFuncs [] = ""
upFuncs [h] = upFunc h
upFuncs (h:t) = upFunc h ++ " \n " ++ upFuncs t

upFunc :: Func -> String
upFunc (Func t s a i) = upType t ++ " " ++ s ++ "(" ++ upArgs a ++ ") { " ++ upBlocoC i ++ " }"

upArgs :: [Arg] -> String
upArgs [] = ""
upArgs [h] = upArg h
upArgs (h:t) = upArg h ++ ", " ++ upArgs t

upArg :: Arg -> String
upArg (Arg t s) = upType t ++ " " ++ s

upFuncCall :: Inst -> String
upFuncCall (CallFunc s a) = s ++ "(" ++ upArgsCall a ++ ")"

upArgsCall :: [ArgCall] -> String
upArgsCall [] = ""
upArgsCall [h] = upArgCall h
upArgsCall (h:t) = upArgCall h ++ ", " ++ upArgsCall t

upArgCall :: ArgCall -> String
upArgCall (ArgCall s) = s

upBlocoC :: BlocoC -> String
upBlocoC [] = ""
upBlocoC [h] = upInst h
upBlocoC (h:t) = upInst h ++ " " ++ upBlocoC t

upForVars :: [Inst] -> String
upForVars [] = ""
upForVars [h] = upForVar h
upForVars (h:t) = upForVar h ++ ", " ++ upForVars t

upForVar :: Inst -> String
upForVar (Atrib var exp) = var ++ " = " ++ upExp1 exp 
upForVar (DeclAtrib t var exp) = upType t ++ " " ++ var ++ " = " ++ upExp1 exp
--upForVar (Decl t var) = upType t ++ " " ++ var

upIf :: Inst -> String
upIf (ITE exp b1 []) = "if (" ++ upExpLogicos exp ++ ") { " ++ upBlocoC b1 ++ " }"
upIf (ITE exp b1 b2) = "if (" ++ upExpLogicos exp ++ ") then { " ++ upBlocoC b1 ++ " } else { " ++ upBlocoC b2 ++ " }"


upInst :: Inst -> String
upInst (Atrib var exp) = var ++ " = " ++ upExp1 exp ++ ";"
upInst (DeclAtrib t var exp) = upType t ++ " " ++ var ++ " = " ++ upExp1 exp ++ ";"
upInst (DeclAtribFuncCall t var f) = upType t ++ " " ++ var ++ " = " ++ upFuncCall f ++ ";"
upInst (AtribFuncCall var f) = var ++ " = " ++ upFuncCall f ++ ";" 
upInst (Decl t var) = upType t ++ " " ++ var ++ ";"
upInst (While exp b) = "while (" ++ upExpLogicos exp ++ ") { " ++ upBlocoC b ++ " }"
upInst (For insts1 exp insts2 b) = "for (" ++ upForVars insts1 ++ "; " ++ upExpLogicos exp ++ "; " ++ upForVars insts2 ++ ") { " ++ upBlocoC b ++ " }" 
upInst (ITE exp b1 b2) = upIf (ITE exp b1 b2)
upInst (Return exp) = "return " ++ upExp1 exp ++ ";"

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
upExpEq (Not a) = "!" ++ upExpEq a
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

