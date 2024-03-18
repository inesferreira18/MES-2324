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
upFuncs (h:t) = upFunc h ++ "  " ++ upFuncs t

upFunc :: Func -> String
upFunc (Func t s a i) = upType t ++ " " ++ s ++ "(" ++ upArgs a ++ "){ " ++ upBlocoC i ++ " }"

upArgs :: [Arg] -> String
upArgs [] = ""
upArgs [h] = upArg h
upArgs (h:t) = upArg h ++ "," ++ upArgs t

upArg :: Arg -> String
upArg (Arg t s) = upType t ++ " " ++ s

upFuncCall :: Inst -> String
upFuncCall (CallFunc s a) = s ++ "(" ++ upArgsCall a ++ ")"

upArgsCall :: [ArgCall] -> String
upArgsCall [] = ""
upArgsCall [h] = upArgCall h
upArgsCall (h:t) = upArgCall h ++ "," ++ upArgsCall t

upArgCall :: ArgCall -> String
upArgCall (ArgCall s) = s

upBlocoC :: BlocoC -> String
upBlocoC [] = ""
upBlocoC [h] = upInst h
upBlocoC (h:t) = upInst h ++ " " ++ upBlocoC t

upForVars :: [Inst] -> String
upForVars [] = ""
upForVars [h] = upForVar h
upForVars (h:t) = upForVar h ++ "," ++ upForVars t

upForVar :: Inst -> String
upForVar (Atrib var exp) = var ++ "=" ++ upExp exp 
upForVar (DeclAtrib t var exp) = upType t ++ " " ++ var ++ "=" ++ upExp exp
--upForVar (Decl t var) = upType t ++ " " ++ var

upIf :: Inst -> String
upIf (ITE exp b1 []) = "if(" ++ upExp exp ++ ") {" ++ upBlocoC b1 ++ "}"
upIf (ITE exp b1 b2) = "if(" ++ upExp exp ++ ") then{" ++ upBlocoC b1 ++ " } else{" ++ upBlocoC b2 ++ "}"


upInst :: Inst -> String
upInst (Atrib var exp) = var ++ "=" ++ upExp exp ++ ";"
upInst (DeclAtrib t var exp) = upType t ++ " " ++ var ++ "=" ++ upExp exp ++ ";"
upInst (DeclAtribFuncCall t var f) = upType t ++ " " ++ var ++ "=" ++ upFuncCall f ++ ";"
upInst (AtribFuncCall var f) = var ++ "=" ++ upFuncCall f ++ ";" 
upInst (Decl t var) = upType t ++ " " ++ var ++ ";"
upInst (While exp b) = "while(" ++ upExp exp ++ ") {" ++ upBlocoC b ++ "}"
upInst (For insts1 exp insts2 b) = "for(" ++ upForVars insts1 ++ "; " ++ upExp exp ++ "; " ++ upForVars insts2 ++ ") {" ++ upBlocoC b ++ "}" 
upInst (ITE exp b1 b2) = upIf (ITE exp b1 b2)
upInst (CallFunc f a) = upFuncCall (CallFunc f a) ++ ";"
upInst (Return exp) = "return " ++ upExp exp ++ ";"

upType :: Type -> String
upType Int = "int"
upType Char = "char"
upType String = "string"
upType Bool = "bool"
upType Void = "void"
 

upExp :: Exp -> String
upExp (And a b) = "(" ++ upExp a ++ " && " ++ upExp b ++ ")"
upExp (Or a b) = "(" ++ upExp a ++ " || " ++ upExp b ++ ")"
upExp (Equal a b) = "(" ++ upExp a ++ " == " ++ upExp b ++ ")"
upExp (Greater a b) = "(" ++ upExp a ++ " > " ++ upExp b ++ ")"
upExp (Less a b) = "(" ++ upExp a ++ " < " ++ upExp b ++ ")"
upExp (GreaterEqual a b) = "(" ++ upExp a ++ " >= " ++ upExp b ++ ")"
upExp (LessEqual a b) = "(" ++ upExp a ++ " <= " ++ upExp b ++ ")"
upExp (Add a b) = "(" ++ upExp a ++ "+" ++ upExp b ++ ")"
upExp (Sub a b) = "(" ++ upExp a ++ "-" ++ upExp b ++ ")"
upExp (Mult a b) = "(" ++ upExp a ++ "*" ++ upExp b ++ ")"
upExp (Div a b) = "(" ++ upExp a ++ "/" ++ upExp b ++ ")"
upExp (Neg (Const a)) = "-" ++ show a
upExp (Const a) = show a
upExp (Var a) = a
upExp (Boolean a) = show a
upExp (Not a) = "(!" ++ upExp a ++ ")"
