module ACLattices where

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap, (!))

import Tree
import AncestralConfigurations ( matchingRootConfigurations, showAC )
import Data.Maybe (fromJust)
import Control.Monad (forM)

type AC = [String]
type Vertex = Int
type EdgeList = [(Vertex, Vertex)]
type AdjList = [[Vertex]]
type LabelMap = HashMap AC Int

-- I wrote these functions at 2 am so they are insane one-liners. here is how it works
-- 1 - select non-leaf lineage from an AC. they can be "split" into two of their children nodes to get one of the new ACs covered by the current one
-- 2 - for each non-leaf lineage, split it into two children lineages using `replaceElementAndFlatten` from Tree.hs
getPrecedingACs :: Tree String -> AC -> [AC]
getPrecedingACs t ac = map (\x -> replaceElementAndFlatten ac x (fromJust $ childrenLabels $ nodeSubtree x t)) $ filter (not . isLeafLabel t) ac

-- 1 - get all ACs in a list
-- 2 - make a "dict" from ACs to their Int labels. we return it so it doesn't need to be recomputed
-- 3 - to make a list of edges, for each of ancestral configurations do
--   a - get all ACs that the current AC covers with `getPrecedingACs`
--   b - make a tuple, encoding the edge to the current AC

getEdgeList :: Tree String -> (LabelMap, EdgeList)
getEdgeList t = (labels, edgelist)
    where aclist = matchingRootConfigurations t
          labels = M.fromList $ zip aclist [1..]
          edgelist = map (bimap (labels !) (labels !)) $ 
                     concatMap (\ac -> map (,ac) $ getPrecedingACs t ac) aclist

getAdjacencyList :: (LabelMap, EdgeList) -> (LabelMap, AdjList)
getAdjacencyList (labels, es) = (labels, forM [1..(length labels)] $ \v -> 
    [y | (x,y) <- es, x == v])
