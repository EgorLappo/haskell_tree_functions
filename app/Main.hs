module Main (main) where

import Tree 
import TreeParse (parseString)
import AncestralConfigurations
import CoalescentHistories

main :: IO ()
main = do
    putStrLn "Try out the functions with GHCi!!"


-- EXAMPLE TREES
t :: Tree String
t = parseString "(((a1,a2),(a3,a4)),a5)"
g :: Tree String
g = parseString "((((l1,l2),l3),l4),(l5,(l6,(l7,l8))))"
s :: Tree String
s = parseString "(((((l1,l2),l3),l4),(l5,l6)),(l7,l8))"
