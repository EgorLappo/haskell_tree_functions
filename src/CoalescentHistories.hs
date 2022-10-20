module CoalescentHistories (countMatchingCoalescentHistories, countNonmatchingCoalescentHistories) where

import Tree (Tree)
import qualified Tree as T

-- data type for m-extended trees
-- either a leaf with an int recording potential subdivision,
-- an internal root with an int recording potential subdivision for the recurrence below
data ExtTree a = Leaf Int a | Node Int a (ExtTree a) (ExtTree a)

-- this is actually a pair of natural transformations between the two functors, but it's irrelevant...

fromTree :: Tree a -> ExtTree a
fromTree (T.Leaf a) = Leaf 1 a -- kind of unnecessary in practice...
fromTree (T.Node a l r) = Node 1 a (fromTree l) (fromTree r)

toTree :: ExtTree a -> Tree a
toTree (Leaf _ a) = T.Leaf a
toTree (Node _ a l r) = T.Node a (toTree l) (toTree r)

labelExt :: ExtTree a -> a
labelExt (Leaf _ a) = a
labelExt (Node _ a _ _) = a

relabelExt :: Int -> ExtTree a -> ExtTree a
relabelExt k (Leaf _ a) = Leaf k a
relabelExt k (Node _ a l r) = Node k a l r

countMatchingCoalescentHistories :: Tree a -> Int
countMatchingCoalescentHistories = countMatchingCoalescentHistories' . fromTree

countNonmatchingCoalescentHistories :: Eq a => Tree a -> Tree a-> Int
countNonmatchingCoalescentHistories g s = countNonmatchingCoalescentHistories' (fromTree g) (fromTree s)

-- Rosenberg (2007), Thm 3.1
countMatchingCoalescentHistories' :: ExtTree a -> Int
countMatchingCoalescentHistories' (Leaf _ _) = 1
countMatchingCoalescentHistories' (Node m _ l r) = sum $ zipWith (*) aL aR
    where aL = [countMatchingCoalescentHistories' $ relabelExt (k+1) l | k <- [1..m]]
          aR = [countMatchingCoalescentHistories' $ relabelExt (k+1) r | k <- [1..m]]

-- Rosenberg (2007), Thm 3.1
countNonmatchingCoalescentHistories' :: Eq a => ExtTree a -> ExtTree a -> Int
countNonmatchingCoalescentHistories' (Leaf _ _) _ = 1
countNonmatchingCoalescentHistories' _ (Leaf _ _) = 1
countNonmatchingCoalescentHistories' (Node _ _ gl gr) s@(Node m _ _ _) = sum $ zipWith (*) aL aR
    where aL = [countNonmatchingCoalescentHistories' gl (relabelExt (k + dl) tl) | k <- [1..m]]
          aR = [countNonmatchingCoalescentHistories' gr (relabelExt (k + dr) tr) | k <- [1..m]]
          tl = (fromTree . T.mrcaNode (toTree s) . T.leafLabels . toTree) gl
          tr = (fromTree . T.mrcaNode (toTree s) . T.leafLabels . toTree) gr
          dl = length $ T.ancestorNodes (labelExt tl) (toTree s)
          dr = length $ T.ancestorNodes (labelExt tr) (toTree s)
