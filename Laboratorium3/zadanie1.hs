{-
parser.hs
Zadanie z parsowania funkcyjnego do wykladu "Specyfikacje formalne
i programy funkcyjne". Marcin Szlenk 2013

Na podstawie:
Graham Hutton, Programming in Haskell, Cambridge University Press, 2007
http://www.cs.nott.ac.uk/~gmh/book.html
("Functional parsing library" i "Expression parser")
-}

import Data.Char

data Parser a = P (String -> [(a, String)])

mul :: [Double] -> Double
mul [] = 1
mul (x:xs) = x * mul xs

-- parser 'return' i operator sekwencji

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

-- podstawowe parsery

item  :: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\inp -> [])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

-- dalsze prymitywy

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

-- wyrazenia arytmetyczne
{-
expr       ::= term [plus_term | minus_term]*
plus_term  ::= '+' term
minus_term ::= '-' term
term       ::= factor [mul_factor | div_factor]*
mul_factor ::= '*' factor
div_factor ::= '/' factor
factor     ::= digit | '(' expr ')'
digit      ::= '0' | '1' | ... | '9'
-}

expr :: Parser Double
expr = do
        t <- term
        p <- many (plus_term +++ minus_term)
        return (t + sum(p))

plus_term = do char '+'
               t <- term
               return t

minus_term = do char '-'
                t <- term
                return (-t)

term :: Parser Double
term = do f <- factor
          m <- many (mul_factor +++ div_factor)
          return (f * mul(m))

mul_factor = do char '*'
                f <- factor
                return f

div_factor = do char '/'
                f <- factor
                return (1/f)

factor :: Parser Double
factor = do d <- digit
            return ((read [d]) :: Double)
           +++
             do char '('
                e <- expr
                char ')'
                return e

eval :: String -> Double
eval inp = case parse expr inp of
              [(n, [])] -> n
              [(_, out)] -> error ("nieskonsumowane " ++ out)
              [] -> error "bledne wejscie"
