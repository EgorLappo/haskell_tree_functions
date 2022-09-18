module ACLattices where

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap, (!))

import Tree
import AncestralConfigurations ( matchingRootConfigurations, showAC )
import Data.Maybe (fromJust)
import Data.List (nub, sortOn)

type AC = [String]
data Vertex = V
    { getAC :: AC
    , getLabel :: Int } deriving (Eq, Show)
type EdgeList = [(Vertex, Vertex)]
type AdjList = [(Vertex, [Vertex])]

-- I wrote these functions at 2 am so they are insane one-liners. here is how it works
-- 1 - select non-leaf lineage from an AC. they can be "split" into two of their children nodes to get one of the new ACs covered by the current one
-- 2 - for each non-leaf lineage, split it into two children lineages using `replaceElementAndFlatten` from Tree.hs
getPrecedingACs :: Tree String -> AC -> [AC]
getPrecedingACs t ac = map (\x -> replaceElementAndFlatten ac x (fromJust $ childrenLabels $ nodeSubtree x t)) $
  filter (not . isLeafLabel t) ac

getEdgeList :: Tree String -> EdgeList
getEdgeList t = map (\(x, y) -> (V x (labels ! x) , V y (labels ! y))) $
                     concatMap (\ac -> map (,ac) $ getPrecedingACs t ac) aclist
    where aclist = matchingRootConfigurations t
          labels = M.fromList $ zip aclist [1..]
 

getAdjacencyList :: EdgeList -> AdjList
getAdjacencyList es = map (\v -> (v, [y | (x,y) <- es, x == v])) $
    sortOn getLabel $ nub $ uncurry (++) $ unzip es

