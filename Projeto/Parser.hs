module Parser where

import Prelude hiding ((<*>), (<$>))
import Data.Char


-- o * tem mais prioridade que o |
infixl 2 <|>
infixl 3 <*>

type Parser r = String -> [(r,String)]


symbol :: Char -> Parser Char
symbol s [] = []
symbol s (h:t) = [(s,t) | h == s]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p [] = []
satisfy p (h:t) = if p h
                  then [(h,t)]
                  else []

token :: [Char] -> Parser [Char]
token t input = if take (length t) input == t
                then [(t,drop (length t) input)] 
                else []

succeed :: a -> String -> [(a,String)]
succeed r input = [(r,input)]

-- succeed 0 "bbb"  (o resultado será 0 as)



-- Combinadores de Parsers!
{- 
    X -> While
        | For
-}

(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) input = p input ++ q input 

pX :: Parser [Char]     
pX = token "While"
  <|> token "For"
  <|> token "while"


{-
    S -> AB
    A -> aA 
      | ε
    B -> b 
      | bB
-}

(<*>) :: Parser (a -> r) -> Parser a -> Parser r
(p <*> q) input = [ (f r,rst') |               
                    (f  ,rst)  <- p input,   
                    (r  ,rst') <- q rst    
                  ]

(<$>) :: (a -> r) -> Parser a -> Parser r
(f <$> p) input = [ (f r,rst) |            
                    (r  ,rst) <- p input   
                  ]

{-
    A -> abca
-}

pA :: Parser Char
pA = f <$> symbol 'a' <*> symbol 'b' <*> symbol 'c' <*> symbol 'a' 
    where f a b c d = b



{-
    As -> a 
        | aA   (a+)
-}

pAs :: Parser Integer
pAs =  f <$> symbol 'a'           
   <|> g <$> symbol 'a' <*> pAs   
   where f x = 1
         g x y = 1 + y             



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
     where f _ '-' d _ = read ('-':d)            
           f _ _ d _ = read d

pSinal :: Parser Char
pSinal = symbol '+'
        <|> symbol '-'
        <|> succeed '+'          

pDigitos :: Parser [Char]
pDigitos =  f <$> satisfy isDigit
        <|> g <$> satisfy isDigit <*> pDigitos
        where f d = [d]
              g d ds = d:ds

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

followedBy p s =     succeed []
              <|> f <$> p <*> s <*> followedBy p s
            where f a _ b = a : b

enclosedBy a c f = (\_ b _ -> b) <$> a <*> c <*> f

pListasIntHaskell :: Parser [Int]
pListasIntHaskell =
     enclosedBy (symbol '[')
                (separatedBy pInt (symbol ','))
                (symbol ']')

blocoCodigoC =
     enclosedBy (symbol '{')
                (followedBy pInt (symbol ';'))
                (symbol '}')

espacos :: Parser [Char]
espacos = zeroOrMore (satisfy isSpace) -- it also validates \n
    
symbol' :: Char -> Parser Char
symbol' a  = (\a b -> a) <$> symbol a <*> espacos

token' :: [Char] -> Parser [Char]
token' t   = (\a b -> a) <$> token t <*> espacos

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' p = (\a b -> a) <$> satisfy p <*> espacos

pNomes :: Parser [Char]
pNomes = f <$> satisfy' isLower <*> zeroOrMore (satisfy isAlphaNum) <*> espacos
       where f a b c = a:b

pTrue :: Parser [Char]
pTrue = (\a _ -> a) <$> token' "True" <*> espacos

pFalse :: Parser [Char]
pFalse = (\a _ -> a) <$> token' "False" <*> espacos




--------------------------------------------------------
-- parser recursivo à esquerda (limitação do parser)
--------------------------------------------------------

pAre :: Parser [Char]
pAre = f <$> pAre <*> symbol 'a'
   <|> g <$> symbol 'a'
   where f a b = a ++ [b]
         g a = [a]