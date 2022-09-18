module AncestralConfigurations (matchingRootConfigurations, rootConfigurations, showAC) where

import Data.List ( intercalate )

import Tree
    ( Tree(..),
      label,
      nodeLabels,
      leafLabels,
      mrcaNode,
      nodeSubtree,
      containsAnyElemFromList )

-- get a list of root ancestral configurations for a tree S
-- (matching pair (S,S))
matchingRootConfigurations :: Eq a => Tree a -> [[a]]
matchingRootConfigurations (Leaf _) = []
matchingRootConfigurations (Node _ l r) = [[label l, label r]]
                                ++ [label l : rs | rs <- matchingRootConfigurations r]
                                ++ [ls ++ [label r] | ls <- matchingRootConfigurations l]
                                ++ [ls ++ rs | ls <- matchingRootConfigurations l, rs <- matchingRootConfigurations r]

rootConfigurations :: Eq a => Tree a -> Tree a -> [[a]]
rootConfigurations g s = filter (not . containsAnyElemFromList antipodals) mACs
    where mACs = matchingRootConfigurations g -- matching root ACs for G. we will filter them using the topology of S
          gNodes = nodeLabels g -- internal nodes of G, of which we select...
          antipodals = filter (isAntipodal g s) gNodes -- ...only nodes which have antipodal pairs among descendants. we need to filter out ACs with these lineages
          isAntipodal :: Eq a => Tree a -> Tree a -> a -> Bool
          isAntipodal g' s' a' = label (mrcaNode s' $ leafLabels $ nodeSubtree a' g') == label s'
-- for the proof that this method works for all nonmatching pairs see https://arxiv.org/abs/2111.10456


showAC :: [String] -> String
showAC ac = "{" ++ intercalate ", " ac ++ "}"
