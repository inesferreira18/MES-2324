module PicoC where

import Parser
import Prelude hiding ((<*>), (<$>))
import Data.Char (isLower)
import Data.Maybe
import Data.Data
import Data.Kind ()

import Tests

data PicoC = PicoC [Inst]
           deriving Show

data Inst = Decl Type String           
          | DeclAtrib Type String Exp  
          | Atrib String Exp                                
          | While Exp BlocoC
          | ITE Exp BlocoC BlocoC
          deriving Show

type BlocoC = [Inst]

data Type = Int
          | Char
          | String
          | Bool
          | Void
          deriving Show

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Neg Exp
         | Const Int
         | Var String
         | Boolean Bool
         | Greater Exp Exp
         | Less Exp Exp
         | Equal Exp Exp
         | GreaterEqual Exp Exp
         | LessEqual Exp Exp
         | And Exp Exp
         | Or Exp Exp
         deriving Show


pPicoC :: Parser PicoC
pPicoC = f <$> pInsts
    where f = PicoC

pInsts :: Parser [Inst]
pInsts = zeroOrMore pInst

pInst :: Parser Inst
pInst =  f  <$> pDeclAtrib <*> symbol' ';' <*> espacos
     <|> f1 <$> pDecl <*> symbol' ';' <*> espacos
     <|> f2 <$> pAtrib <*> symbol' ';' <*> espacos
     <|> g  <$> pWhile <*> espacos
     <|> h  <$> pITE <*> espacos
    where f a b c = a
          f1 a b c = a
          f2 a b c = a  
          g a b = a
          h a b = a

pBlocoC :: Parser BlocoC
pBlocoC = f <$> symbol' '{' <*> pInsts <*> symbol' '}'
    where f a b c = b

pDecl :: Parser Inst
pDecl = f <$> pType <*> pNomes
    where f a b = Decl a b

pDeclAtrib :: Parser Inst
pDeclAtrib = f <$> pType <*> pNomes <*> symbol' '=' <*> pExp1
    where f a b c d = DeclAtrib a b d

pAtrib :: Parser Inst
pAtrib = f <$> pNomes <*> symbol' '=' <*> pExp1
       where f a b c = Atrib a c

pWhile :: Parser Inst
pWhile = f <$> token' "while" <*> symbol' '(' <*> pExpLogicos <*> symbol' ')' <*> pBlocoC
    where f a b c d e = While c e

pITE:: Parser Inst
pITE = f <$> token' "if" <*> symbol' '(' <*> pExpLogicos <*> symbol' ')' <*> token' "then" <*> pBlocoC <*> token' "else" <*> pBlocoC
    where f a b c d e g h i = ITE c g i

pType :: Parser Type
pType = f <$> token' "int"
    <|> g <$> token' "char"
    <|> h <$> token' "string"
    <|> i <$> token' "bool"
    <|> j <$> token' "void"
    where f a = Int
          g a = Char
          h a = String
          i a = Bool
          j a = Void

pExpLogicos :: Parser Exp
pExpLogicos =  f <$> pExpEq <*> token' "&&" <*> pExpLogicos
           <|> g <$> pExpEq <*> token' "||" <*> pExpLogicos
           <|> h <$> pExpEq
           where f a b c = And a c
                 g a b c = Or a c
                 h a = a

pExpEq :: Parser Exp
pExpEq = f <$> pExp1 <*> token' "==" <*> pExpEq
     <|> g <$> pExp1 <*> symbol' '>' <*> pExpEq
     <|> h <$> pExp1 <*> symbol' '<' <*> pExpEq
     <|> i <$> pExp1 <*> token' ">=" <*> pExpEq
     <|> j <$> pExp1 <*> token' "<=" <*> pExpEq
     <|> k <$> pExp1
     where f a b c = Equal a c
           g a b c = Greater a c
           h a b c = Less a c
           i a b c = GreaterEqual a c
           j a b c = LessEqual a c
           k a = a

pExp1 :: Parser Exp
pExp1 = f <$> pExp0 <*> symbol' '+' <*> pExp1  
    <|> g <$> pExp0 <*> symbol' '-' <*> pExp1
    <|> h <$> pExp0
    where f a b c = Add a c
          g a b c = Sub a c
          h a = a

pExp0 :: Parser Exp
pExp0 = f <$> pFactor <*> symbol' '*' <*> pExp0
    <|> g <$> pFactor <*> symbol' '/' <*> pExp0
    <|> h <$> pFactor
    where f a b c = Mult a c
          g a b c = Div a c
          h a = a

pFactor :: Parser Exp
pFactor =  f <$> pInt
       <|> g <$> pTrue
       <|> h <$> pFalse
       <|> i <$> pNomes
       <|> j <$> symbol' '(' <*> pExp1 <*> symbol' ')'
       where f a = Const a
             g a = Boolean True
             h a = Boolean False
             i a = Var a
             j a b c = b




--------------------------------------------------------
-- Eval Function
--------------------------------------------------------

-- eval ast [("aux1", 10)]
-- eval (GreaterEqual (Add (Const 3) (Const 5)) (Sub (Const 10) (Var "aux"))) [("aux",2)] -- True
eval :: Exp -> [(String,Int)] -> Int
eval (Const i) _ = i
eval (Var n) c = fromJust (lookup n c)                      -- lookup vai buscar o valor da variável no contexto c; fromjust coverte aquilo que o lookup devolve
eval (Neg e) c = - (eval e c)
eval (Add e1 e2) c = eval e1 c + eval e2 c
eval (Sub e1 e2) c = eval e1 c - eval e2 c
eval (Mult e1 e2) c = eval e1 c * eval e2 c
eval (Div e1 e2) c = eval e1 c `div` eval e2 c
eval (Equal e1 e2) c = fromEnum $ eval e1 c == eval e2 c    -- fromEnum converte um booleano para um inteiro
eval (Greater e1 e2) c = fromEnum $ eval e1 c > eval e2 c
eval (Less e1 e2) c = fromEnum $ eval e1 c < eval e2 c
eval (GreaterEqual e1 e2) c = fromEnum $ eval e1 c >= eval e2 c
eval (LessEqual e1 e2) c = fromEnum $ eval e1 c <= eval e2 c









-- eval ast2 []
-- eval (opt ast2) []
ast2 :: Exp
ast2 = Mult (Add (Const 3) (Const 0)) (Const 5)

-- opt ast3
ast3 :: Exp
ast3 = Add (Add (Neg (Const 4)) (Const 4)) (Const 5)  -- 5

ast5 :: Exp
ast5 = GreaterEqual (Const 3) (Const 5)  -- Bool False

-- pExp1 "3 + aux1 / 5"
ast :: Exp
ast = Add (Const 3) (Div (Var "aux1") (Const 5))

ast4 :: Exp
ast4 = Div (Add (Const 3) (Const 7)) (Const 5)
