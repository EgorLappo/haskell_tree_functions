{-# LANGUAGE TupleSections #-}

module ACLAttices ( Vertex
                  , AC
                  , EdgeList
                  , AdjList
                  , getEdgeList
                  , getAdjacencyList
                  )  where

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import Tree
import AncestralConfigurations ( matchingRootConfigurations, showAC )
import Data.Maybe (fromJust)
import Data.List (nub, sort, sortOn)
import qualified Data.Array as A
import Data.Array (Array, (!))

type AC = [String] 
data Vertex = V
    { getAC :: AC
    , getLabel :: Int } deriving (Eq, Show)
type EdgeList = [(Vertex, Vertex)]
type AdjList = [(Vertex, [Vertex])]
type Path = [Vertex]

-- I wrote these functions at 2 am so they are insane one-liners. here is how it works
-- 1 - select non-leaf lineage from an AC. they can be "split" into two of their children nodes to get one of the new ACs covered by the current one
-- 2 - for each non-leaf lineage, split it into two children lineages using `replaceElementAndFlatten` from Tree.hs
getPrecedingACs :: Tree String -> AC -> [AC]
getPrecedingACs t ac = map (\x -> replaceElementAndFlatten ac x (fromJust $ childrenLabels $ nodeSubtree x t)) $
  filter (not . isLeafLabel t) ac

getEdgeList :: Tree String -> EdgeList
getEdgeList t = map (\(x, y) -> (V x (labels H.! x) , V y (labels H.! y))) $
                     concatMap (\ac -> map (,ac) $ getPrecedingACs t ac) aclist
          -- we sort the lineages in the ACs to ensure consistent representatiion
    where aclist = map sort $ matchingRootConfigurations t 
          labels = M.fromList $ zip aclist [1..]
 
getAdjacencyList :: Tree String -> AdjList
getAdjacencyList = toAdjacencyList . getEdgeList

toAdjacencyList :: EdgeList -> AdjList
toAdjacencyList es = map (\v -> (v, [y | (x,y) <- es, x == v])) $
    sortOn getLabel $ nub $ uncurry (++) $ unzip es

toEdgeList :: AdjList -> EdgeList
toEdgeList = concatMap (\(v, vs) -> map (v,) vs)

cartProd :: EdgeList -> EdgeList -> EdgeList 
cartProd xs ys = [mkEdge e e' | e <- xs, e' <- ys]
  where mkEdge (a, b) (a', b') = (mkNode a a', mkNode b b')
        mkNode a b = V (mkAC a b) (mkLabel a b)
        mkAC (V l _) (V l' _) = sort (l ++ l')
        mkLabel (V _ i) (V _ j) = i*n + j
        n = length $ vertices xs

vertices :: EdgeList -> [Vertex]
vertices = nub . concat . unzip 


