{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

{-# DeriveDataTypeable #-}


import Parser
import Prelude hiding ((<*>), (<$>))
import Data.Char (isLower)
import Data.Maybe
import Data.Data

--import Data.Generics.Zipper
import Library.Ztrategic
--import Library.StrategicData (StrategicData)

instance StrategicData Int
instance StrategicData a => StrategicData.StrategicData [a]

instance StrategicData PicoC

-- completar o parser
-- completar a função unparser
-- melhorar a função opt para fazer todas as optimizacoes

-- testar a propriedade definida pela função prop
--   prop :: PicoC -> Bool
--   prop ast = ast == parser (unparse ast)
-- (precisam usar deriving Eq nos datas types)

data PicoC = PicoC [Inst]
           deriving (Data,Show)


data Inst = Atrib String Exp
         | While Exp BlocoC
         | ITE Exp BlocoC BlocoC
          deriving (Data,Show)


type BlocoC = [Inst]



pPicoC :: Parser PicoC
pPicoC = f <$> zeroOrMore pInst
    where f a = PicoC a

pInst :: Parser Inst
pInst =  pAtrib <*> symbol' ';'
     <|> pWhile <*> symbol' ';'
     <|> pITE <*> symbol' ';'

pAtrib :: Parser Inst
pAtrib = f <$> pNomes <*> symbol' '=' <*> pExp1
       where f a b c = Atrib a c

pWhile :: Parser Inst
pWhile = f <$> token "while" <*> pExpEq <*> pBlocoC
    where f a b c = While b c

pITE:: Parser Inst
pITE = f <$> token "if" <*> pExpEq <*> token "then" <*> pBlocoC <*> token "else" <*> pBlocoC
    where f a b c d e f = ITE b d f                                                                                                                                                            



pBlocoC :: Parser BlocoC
pBlocoC = enclosedBy (symbol' '{') (zeroOrMore pInst) (symbol' '}')







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
upFactor (Bool a) = show a
upFactor (Var a) = a
upFactor e = "(" ++ upExp1 e ++ ")"


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
--eval (Bool b) _ = fromEnum b                                    -- permite fazer 1 > True









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




{-
ghci> opt ast3
Add (Add (Const (-4)) (Const 4)) (Const 5)
ghci> opt $ opt ast3
Add (Const 0) (Const 5)
ghci> opt $ opt $ opt ast3
Const 5
ghci> opt $ opt $ opt $ opt ast3
Const 5

Ponto Fixo!
-}



examplePicoC = "int margem = 15; \n if (margem > 30) \n then { margem = 4 * 23 + 3 ; } \n else { margem = 0; }"
example = PicoC [Atrib "margem" (Const 15), ITE (Const 30) [Atrib "x" (Sub (Var "x") (Const 1))]]

{-

t = toZipper examplePicoC
gethole t :: Maybe PicoC 

Just f1 = down' t

gethole f1 :: Maybe [Inst]
Just attrib = down' f1


Mesmo que isto...
-}


ex3 = let t = toZipper examplePicoC
          Just f1 = down' t
          Just attrib = down' f1
          Just rattrib = right attrib
          Just ite = down' rattrib 
          --(...)
          in getHole attrib :: Maybe Inst


alteraLista :: [Int]
alteraListaS = 
    let listaZipper = toZipper lista
        Just listaNova = applyTp (full_tdTP step) listaZipper
             where step = idTP `adhocTP` alteraDois
    in
        fromZipper listaNova

alteraDois :: Int -> Maybe Int
alteraDois 2 = Just 12
alteraDois x = Just x



--etiquetaVars example
etiquetaVars :: PicoC -> PicoC
etiquetaVars p = 
    let pZipper = toZipper p
        Just newP = applyTp (full_tdTP step) pZipper
        step = idTP `adhocTP` etiqueta `adhocTP` etiquetaAtrib
    in fromZipper newP

etiqueta :: Exp -> Maybe Exp
etiqueta (Var s) = Just (Var ("v_" ++ s))
etiqueta x = Just x

etiquetaAtrib :: Inst -> Maybe Inst
etiquetaAtrib (Atrib s e) = Just (Atrib ("v_" ++ s) e)
etiquetaAtrib x = Just x



--etiquetaVars2 examplePicoC
etiquetaVars2 :: PicoC -> PicoC
etiquetaVars2 p = 
    let pZipper = toZipper p
        Just newP = applyTp (full_tdTP step) pZipper
        step = idTP `adhocTP` etiquetaString
    in fromZipper newP

etiquetaString :: String -> Maybe String
etiquetaString s = Just ("v_" ++ s)       -- Loop infinito
etiquetaString x = Just x





etiquetaVars3 :: PicoC -> PicoC
etiquetaVars3 p = 
    let pZipper = toZipper p
        Just newP = applyTp (full_buTP step) pZipper
        step = idTP `adhocTP` etiqueta `adhocTP` etiquetaAtrib
    in fromZipper newP

--once_bu não faz nada porque só aplica o step uma vez. Parou porque o tipo de dados das etiquetas é Maybe e devolveu Just qualquer coisa

