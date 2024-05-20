{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveDataTypeable #-}
module PicoC where

import Prelude hiding ((<*>), (<$>))
import Data.Maybe
import Data.Data

import Data.Generics.Zipper

import Parser
import Tests

data PicoC = PicoC [Func]
           deriving (Show, Data)

data Func = Func Type String [Arg] [Inst]
          deriving (Show,Data)

data Arg = Arg Type String
         deriving (Show,Data)

data ArgCall = ArgCall String
         deriving (Show,Data)

data Inst = Decl Type String
          | DeclAtrib Type String Exp
          | Atrib String Exp
          | Print String
          | DeclAtribFuncCall Type String Inst
          | AtribFuncCall String Inst
          | While Exp BlocoC
          | For [Inst] Exp [Inst] BlocoC        -- for (int i=0, j=0; i < 10; i++, j++ ) { ... }
          | ITE Exp BlocoC BlocoC
          | CallFunc String [ArgCall]
          | Return Exp
          deriving (Show,Data)

type BlocoC = [Inst]

data Type = Int
          | Char
          | String
          | Bool
          | Void
          deriving (Show,Data)

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
         | Not Exp
         deriving (Show,Data)


pPicoC :: Parser PicoC
pPicoC = f <$> pFuncs
    where f = PicoC

pFuncs :: Parser [Func]
pFuncs = zeroOrMore pFunc

pFunc :: Parser Func  --     int main() { int x; x = 2; }   ou    int main(char a, int b) { int x; x = 2; }
pFunc = f <$> espacos <*> pType <*> pNomes <*> symbol' '(' <*> pArgs <*> symbol' ')' <*> symbol' '{' <*> espacos <*> pInsts <*> symbol' '}'
    where f a b c d e f g h i j = Func b c e i

pFuncCall :: Parser Inst
pFuncCall = f <$> pNomes <*> symbol' '(' <*> pArgsCall <*> symbol' ')'
    where f a b c d = CallFunc a c

pArgs :: Parser [Arg]
pArgs = f <$> pArg
    <|> g <$> pArg <*> symbol' ',' <*> pArgs
    <|> succeed []
    where f a = [a]
          g a b c = a:c

pArg :: Parser Arg
pArg = f <$> pType <*> pNomes
    where f a b = Arg a b

pArgsCall :: Parser [ArgCall]
pArgsCall = f <$> pArgCall
        <|> g <$> pArgCall <*> symbol' ',' <*> pArgsCall
        <|> succeed []
        where f a = [a]
              g a b c = a:c

pArgCall :: Parser ArgCall
pArgCall = f <$> pNomes
    where f a  = ArgCall a

pPrint:: Parser Inst
pPrint = f <$> token' "print(" <*> pString <*> token' ")"
    where f a b c = Print b

pInsts :: Parser [Inst]
pInsts = zeroOrMore pInst

pInst :: Parser Inst
pInst =  f  <$> pDeclAtrib <*> symbol' ';' <*> espacos
     <|> f1 <$> pDecl <*> symbol' ';' <*> espacos
     <|> f2 <$> pAtrib <*> symbol' ';' <*> espacos
     <|> g  <$> pWhile <*> espacos
     <|> h  <$> pFor <*> espacos
     <|> i  <$> pITE <*> espacos
     <|> p  <$> pPrint <*> symbol' ';' <*> espacos
     <|> l  <$> pFuncCall <*> symbol' ';' <*> espacos
     <|> k  <$> pDeclAtribFuncCall <*> symbol' ';' <*> espacos
     <|> m  <$> pAtribFuncCall <*> symbol' ';' <*> espacos
     <|> j  <$> pReturn <*> symbol' ';' <*> espacos
     where f a b c = a
           f1 a b c = a
           f2 a b c = a
           g a b = a
           h a b = a
           i a b = a
           p a b c = a
           k a b c = a
           l a b c = a
           m a b c = a
           j a b c = a

pBlocoC :: Parser BlocoC
pBlocoC = f <$> symbol' '{' <*> pInsts <*> symbol' '}'
    where f a b c = b

pDeclAtribFuncCall :: Parser Inst
pDeclAtribFuncCall = f <$> pType <*> pNomes <*> symbol' '=' <*> pFuncCall
          where f a b c d = DeclAtribFuncCall a b d

pAtribFuncCall :: Parser Inst
pAtribFuncCall = f <$> pNomes <*> symbol' '=' <*> pFuncCall
          where f a b c = AtribFuncCall a c

pAtribs :: Parser Inst
pAtribs = pAtrib
        <|> pDeclAtrib

pDeclAtrib :: Parser Inst
pDeclAtrib = f <$> pType <*> pNomes <*> symbol' '=' <*> pExp1
          where f a b c d = DeclAtrib a b d
                g a b c d = DeclAtrib a b d

pAtrib :: Parser Inst
pAtrib = f <$> pNomes <*> symbol' '=' <*> pExp1
       where f a b c = Atrib a c

pDecl :: Parser Inst
pDecl = f <$> pType <*> pNomes
    where f a b = Decl a b


pForInits :: Parser [Inst]
pForInits =  f <$> pAtribs
         <|> g <$> pAtribs <*> symbol' ',' <*> pForInits  -- for(int i=0, j=0; ...)
         <|> succeed []
         where f a = [a]
               g a b c = a:c

pForIncs :: Parser [Inst]
pForIncs =  f <$> pAtrib
        <|> g <$> pAtrib <*> symbol' ',' <*> pForIncs   -- for(...; ...; i = i+1, j=j-1)
        <|> succeed []
        where f a = [a]
              g a b c = a:c

pWhile :: Parser Inst
pWhile = f <$> token' "while" <*> symbol' '(' <*> pExpLogicos <*> symbol' ')' <*> pBlocoC
    where f a b c d e = While c e

pFor :: Parser Inst
pFor = f <$> token' "for" <*> symbol' '(' <*> pForInits <*> symbol' ';' <*> pExpLogicos <*> symbol' ';' <*> pForIncs <*> symbol' ')' <*> pBlocoC
    where f a b c d e f g h i  = For c e g i

pITE:: Parser Inst
pITE = f <$> token' "if" <*> symbol' '(' <*> pExpLogicos <*> symbol' ')' <*> token' "then" <*> pBlocoC <*> token' "else" <*> pBlocoC
    <|> g <$> token' "if" <*> symbol' '(' <*> pExpLogicos <*> symbol' ')' <*> pBlocoC
    where f a b c d e g h i = ITE c g i
          g a b c d e = ITE c e []

pReturn :: Parser Inst
pReturn = f <$> token' "return" <*> pExpLogicos
    where f a b = Return b

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
           <|> h <$> token' "!" <*> pExpLogicos
           <|> i <$> pExpEq
           where f a b c = And a c
                 g a b c = Or a c
                 h a b = Not b
                 i a = a

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
pFactor =  f1 <$> symbol' '-' <*> pInt
       <|> f2 <$> pInt
       <|> g <$> pTrue
       <|> h <$> pFalse
       <|> i <$> pNomes
       <|> j <$> symbol' '(' <*> pExpLogicos <*> symbol' ')'
       where f1 a b = Neg (Const b)
             f2 a = Const a
             g a = Boolean True
             h a = Boolean False
             i a = Var a
             j a b c = b
