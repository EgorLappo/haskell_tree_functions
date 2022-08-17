module TreeParse where

import Control.Applicative
import Data.Char
import Tree

placeholderLabel = "x"

nameInternalNodes :: Int -> String -> String 
nameInternalNodes i [] = []
nameInternalNodes i [s] | s == ')' = ")" ++ placeholderLabel ++ show i
                     | otherwise = [s]
nameInternalNodes i (x:y:xs) | x == ')' = if (y == ',') || (y == ')') 
                                          then x:(placeholderLabel ++ show i ++ nameInternalNodes (i+1) (y:xs)) 
                                          else (x : nameInternalNodes i (y:xs))
                             | otherwise = x : nameInternalNodes i (y:xs)

-- parse the Newick string into a Tree
parseNewick :: String -> Tree String
parseNewick = fst . head . (parse tree) . (nameInternalNodes 1)

-- the parser code is straight from the Hutton textbook
-- i don't need more complicated stuff for now
-- possible future update is to automatically detect unlabeled nodes

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- ** type class definitions **
instance Functor Parser where
    fmap g (P p) = P (\inp -> case p inp of [] -> [] 
                                            [(v, rem)] -> [(g v, rem)])

instance Applicative Parser where 
    pure x = P (\inp -> [(x, inp)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> p = P (\inp -> 
        case parse pg inp of [] -> []
                             [(pf, rem)] -> parse (fmap pf p) rem)
                                
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> 
        case parse p inp of [] -> []
                            [(v, rem)] -> parse (f v) rem)

instance Alternative Parser where
    empty = P (\_ -> [])
    p <|> q = P (\inp -> 
        case parse p inp of [] -> parse q inp
                            [(v, rem)] -> [(v, rem)])

item :: Parser Char
item = P (\inp -> case inp of [] -> []
                              (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat f = do x <- item
           if f x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

alphanum :: Parser Char
alphanum = sat isAlphaNum

leafLabel :: Parser (Tree String)
leafLabel = do xs <- some alphanum
               return (Leaf xs)


tree :: Parser (Tree String)
tree = do char '('
          l <- tree <|> leafLabel
          char ','
          r <- tree <|> leafLabel
          char ')'
          x <- some alphanum
          return (Node x l r)
       <|> leafLabel

    
          
