module Main where

import Tree 
import TreeParse
import AncestralConfigurations

-- a module to just gather stuff for use in GHCi

main :: IO ()
main = putStrLn "Use the functions in the modules from GHCi!"

-- EXAMPLE TREES
t = parseNewick "(((a1,a2),(a3,a4)),a5)"
g = parseNewick "((((l1,l2),l3),l4),(l5,(l6,(l7,l8))))"
s = parseNewick "((((l1,l2),l3),l4),((l5,l6),(l7,l8)))"