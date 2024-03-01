{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

module Parser where

import Prelude hiding ((<*>), (<$>))
import Data.Char


-- o * tem mais prioridade que o |
infixl 2 <|>
infixl 3 <*>

-- Parser é uma função que recebe um texto e devolve uma lista de pares (resultado,resto da string)
type Parser r = String -> [(r,String)] -- r é o tipo do resultado do parser; lista vazia para quand dá erro de parse  


symbol :: Char -> Parser Char
symbol s [] = []
symbol s (h:t) = [(s,t) | h == s] -- list compreension

-- ghci parser.hs
-- symbol 'b' "baaaa"                



-- Quero ver se o simbolo à cabeça satisfaz um predicado (função)
-- Recebe um predicado e um texto de entrada
satisfy :: (Char -> Bool) -> Parser Char
satisfy p [] = []
satisfy p (h:t) = if p h
                  then [(h,t)]
                  else []

-- :r
-- satisfy isDigit "aa"
-- satisfy isDigit "1aa"



--token :: [Char] -> String -> [([Char],String)]
token :: [Char] -> Parser [Char]
token t input = if take (length t) input == t
                then [(t,drop (length t) input)] -- resultado do parser, o que ficou por processar
                else []

-- token "while" "while (x > 0)...."s



succeed :: a -> String -> [(a,String)]
succeed r input = [(r,input)]

-- succeed 0 "bbb"  (o resultado será 0 as)



-- Combinadores de Parsers!
{- 
    X -> While
        | For
-}
-- () indica um operador infixo

(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) input = p input ++ q input -- concatena o parser que funcionar com o que não funcionar (lista vazia)

pX :: Parser [Char]     -- aqui não preciso de especificar o argumento de entrada dado que este já está especificado na função token. O Haskell infere o tipo através da análise do contexto (currying)
pX = token "While"
  <|> token "For"
  <|> token "while"

-- pX "For (x > 0)...."



{-
    S -> AB
    A -> aA 
      | ε
    B -> b 
      | bB
-}

-- Esta função é utilizada para aplicar um parser que produz uma função a um parser que produz um argumento. 
-- Devolve um parser que produz o resultado de aplicar a função ao argumento.
(<*>) :: Parser (a -> r) -> Parser a -> Parser r
(p <*> q) input = [ (f r,rst') |           -- o resultado do parser é f r e o resto é rst'    
                    (f  ,rst)  <- p input, -- o parser p produz uma função f e um resto rst;  
                    (r  ,rst') <- q rst    -- o parser q produz um resultado r e um resto rst';
                  ]



(<$>) :: (a -> r) -> Parser a -> Parser r
(f <$> p) input = [ (f r,rst) |            -- aplico a função f ao resultado r do parser p
                    (r  ,rst) <- p input   -- r é o resultado do parser p aplicado à entrada input
                  ]

{-
    A -> abca
-}

pA :: Parser Char
pA = f <$> symbol 'a' <*> symbol 'b' <*> symbol 'c' <*> symbol 'a' -- f <$> é um functor
    where f a b c d = b



{-
    As -> a 
        | aA   (a+)
-}

pAs :: Parser Integer
pAs =  f <$> symbol 'a'           -- p[0] = p[1] em python
   <|> g <$> symbol 'a' <*> pAs   -- p[0] = p[1] + p[2] em python         f e g têm que ter o mesmo tipo
   where f x = 1
         g x y = 1 + y            -- este parser conta os as 

-- pAs "aaaa" -- o último parser é o correto: (4, "")



{-
    Int -> Sinal Digitos

    Sinal -> ε 
           | + 
           | -

    Digitos -> dig
            | dig Digitos
-}

pInt :: Parser Int
pInt = f <$> espacos <*> pSinal <*> pDigitos <*> espacos
     where f _ '-' d _ = read ('-':d)            -- read converte uma string num inteiro
           f _ _ d _ = read d

pSinal :: Parser Char
pSinal = symbol '+'
        <|> symbol '-'
        <|> succeed '+'          -- se não houver sinal, então é positivo

pDigitos :: Parser [Char]
pDigitos =  f <$> satisfy isDigit
        <|> g <$> satisfy isDigit <*> pDigitos
        where f d = [d]
              g d ds = d:ds

-- pInt "+123" resultado: ("+123","")


oneOrMore :: Parser a -> Parser [a]
oneOrMore p =   f <$>  p
           <|>  g  <$> p <*> oneOrMore p
   where f x = [x]
         g x y = x : y

pString :: Parser [Char]
pString = f <$> symbol '\"' <*>
                zeroOrMore (satisfy (/= '\"')) <*>
                symbol '\"'
   where f a b c = b

ex :: [([Char], String)]
ex = pString "\"abcd  ajajd29309283092 e\""


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p  =        succeed []
             <|> f <$> p <*> zeroOrMore p
             where f x y = x:y

optional :: Parser a -> Parser [a]
optional p =   f <$>  p
          <|>         succeed []
          where f a = [a]

sinal :: Parser Char
sinal =  symbol '+'
     <|> symbol '-'

pInt' :: Parser [Char]
pInt' =  f <$> optional sinal <*>
               oneOrMore (satisfy isDigit)
     where f a b = a ++ b

separatedBy :: Parser a1 -> Parser a2 -> Parser [a1]
separatedBy p s =  f <$> p
               <|> g <$> p  <*> s <*> separatedBy p s
   where f a     = [a]
         g a b c = a : c;


followedBy :: Parser a1 -> Parser a2 -> Parser [a1]
followedBy p s =     succeed []
              <|> f <$> p <*> s <*> followedBy p s
            where f a _ b = a : b



enclosedBy :: Parser a1 -> Parser r -> Parser a2 -> Parser r
enclosedBy a c f = (\_ b _ -> b) <$>  a <*> c <*> f



pListasIntHaskell :: Parser [Int]
pListasIntHaskell =
     enclosedBy (symbol '[')
                (separatedBy pInt (symbol ','))
                (symbol ']')

blocoCodigoC :: Parser [Int]
blocoCodigoC =
     enclosedBy (symbol '{')
                (followedBy pInt (symbol ';'))
                (symbol '}')

espacos :: Parser [Char]
espacos = zeroOrMore (satisfy isSpace)

newlines :: Parser [Char]
newlines = zeroOrMore (satisfy isNewline)
    where isNewline c = c == '\n'

symbol' :: Char -> Parser Char
symbol' a  = (\a b -> a) <$> symbol a <*> espacos

token' :: [Char] -> Parser [Char]
token' t   = (\a b -> a) <$> token t <*> espacos

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' p = (\a b -> a) <$> satisfy p <*> espacos

pNomes = f <$> satisfy' isLower <*> zeroOrMore (satisfy' isAlphaNum) <*> espacos
       where f a b c = a:b



--------------------------------------------------------
-- parser recursivo à esquerda (limitação do parser)
--------------------------------------------------------

pAre :: Parser [Char]
pAre = f <$> pAre <*> symbol 'a'
   <|> g <$> symbol 'a'
   where f a b = a ++ [b]
         g a = [a]