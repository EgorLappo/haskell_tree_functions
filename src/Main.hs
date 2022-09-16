module Main where

import Data.Maybe

import Tree 
import TreeParse (parseString)
import AncestralConfigurations
import CoalescentHistories

-- a module to just gather stuff for use in GHCi

main :: IO ()
main = putStrLn "Use the functions in the modules from GHCi!"

-- EXAMPLE TREES
t = parseString "(((a1,a2),(a3,a4)),a5)"
g = parseString "((((l1,l2),l3),l4),(l5,(l6,(l7,l8))))"
s = parseString "(((((l1,l2),l3),l4),(l5,l6)),(l7,l8))"