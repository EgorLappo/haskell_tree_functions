module ACLattices where

import Data.Array
import Data.HashMap

import Tree
import AncestralConfigurations

newtype DAG = DAG { labels}
