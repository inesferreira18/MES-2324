{-# HLINT ignore "Eta reduce" #-}

module Exp where

import Parser
import Prelude hiding ((<*>),(<$>))

{-
    ExpEq -> Exp1 "==" Exp1                 -- ou Exp1 "==" ExpEq            -- == têm menos prioridade que os outros????
          |  Exp1 ">" Exp1                  -- ou Exp1 ">" ExpEq
          |  Exp1 "<" Exp1
          |  Exp1 ">=" Exp1
          |  Exp1 "<=" Exp1
          |  Exp1

    Exp1 -> Exp0 '+' Exp1
        |  Exp0 '-' Exp1
        |  Exp0

    Exp0 -> pFactor '*' Exp0
        |  pFactor '/' Exp0
        |  pFactor

    pFactor -> int
            |  true
            |  false
            |  var
            |  '('  Exp1 ')'

-}


data Exp = Add Exp Exp
        | Sub Exp Exp
        | Mult Exp Exp
        | Div Exp Exp
        | Neg Exp
        | Const Int
        | Var String
        | Bool Bool
        | Greater Exp Exp
        | Less Exp Exp
        | Equal Exp Exp
        | GreaterEqual Exp Exp
        | LessEqual Exp Exp
        deriving Show



pExpEq :: Parser Exp
pExpEq = f <$> pExp1 <*> token' "==" <*> pExp1
     <|> g <$> pExp1 <*> symbol' '>' <*> pExp1
     <|> h <$> pExp1 <*> symbol' '<' <*> pExp1
     <|> i <$> pExp1 <*> token' ">=" <*> pExp1
     <|> j <$> pExp1 <*> token' "<=" <*> pExp1
     <|> k <$> pExp1
     where f a b c = Equal a c
           g a b c = Greater a c
           h a b c = Less a c
           i a b c = GreaterEqual a c
           j a b c = LessEqual a c
           k a = a

pExp1 :: Parser Exp
pExp1 = f <$> pExp0 <*> symbol' '+' <*> pExp1   -- p[0] = p[1] + p[2]
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
       <|> h <$> token "true"
       <|> i <$> token "false"
       <|> g <$> pNomes
       <|> j <$> symbol' '(' <*> pExp1 <*> symbol' ')'
       where f a = Const a
             h a = Bool True
             i a = Bool False
             g a = Var a
             j a b c = b