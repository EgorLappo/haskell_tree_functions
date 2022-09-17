module ACLattices where

import Data.Array
import Data.HashMap.Strict

import Tree
import AncestralConfigurations
import Data.List (intercalate)

newtype AC = AC [String] deriving Eq

instance Show AC where
    show (AC ac) = "{" ++ intercalate ", " ac ++ "}"

type Vertex = Int
data DAG = DAG 
    { labels :: Array Vertex AC
    , aList  :: Array Vertex [Vertex] }
