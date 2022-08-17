module Tree where 

import Data.List ( nub ) 
import Control.Applicative ( Alternative((<|>)) )

    
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Eq

-- get all leaf labels
leafLabels :: Tree a -> [a]
leafLabels (Leaf a) = [a]
leafLabels (Node a l r) = leafLabels l ++ leafLabels r

-- get labels of all nodes in a list
nodeLabelList :: Tree a -> [a]
nodeLabelList (Leaf a) = [a]
nodeLabelList (Node a l r) = nodeLabelList l ++ [a] ++ nodeLabelList r

-- predicate to check if the nodes of a tree are labeled uniquely
isUniqueLabeled :: Eq a => Tree a -> Bool
isUniqueLabeled t = l == nub l where l =  nodeLabelList t

treeSize :: Tree a -> Int
treeSize (Leaf _) = 1
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

-- ** ALL OPERATIONS BELOW ARE FOR UNIQUELY LABELED TREES ** 
-- ** ALL OPERATIONS ASSUME THAT GIVEN LABELS DO EXISt IN THE TREE ** 

-- get most recent common ancestor (MRCA) of a given list of leaf (labels)
mrcaNode :: Eq a => [a] -> Tree a -> Tree a
mrcaNode ls t 
    | not $ isUniqueLabeled t = error "the tree is not labeled uniquely!"
    | otherwise = case mrcaNode' ls t of 
        Just t' -> t' 
        Nothing -> error "leaf labels do not match the tree!"


mrcaNode' :: Eq a => [a] -> Tree a -> Maybe (Tree a)
mrcaNode' [l] (Leaf l') = if l == l' then Just (Leaf l) else Nothing -- first match on leaves, which happens only if the list has a single element
mrcaNode' _   (Leaf y)  = Nothing                                    -- bigger lists never match
mrcaNode' ls t@(Node a l r) = if isListSubset ls $ nodeLabelList t   -- if the labels in the list are in subtree, then we are on the right track, we only need to ensure minimality
                              then case (isListSubset ls $ nodeLabelList l, isListSubset ls $ nodeLabelList r) of 
                                        (False, False) -> Just t -- this means this node is minimal, return it; otherwise recurse
                                        (True, False) -> mrcaNode' ls l
                                        (False, True) -> mrcaNode' ls r 
                                        -- last option (True, True) is not possible as leaf sets of subtrees are guaranteed to be disjoint
                              else Nothing

-- get labels of all ancestors of a given node
ancestorNodes :: Eq a => a -> Tree a -> [a]
ancestorNodes a t
    | not $ isUniqueLabeled t = error "the tree is not labeled uniquely!"
    | otherwise = case ancestorNodes' a t of 
        Just ls -> ls
        Nothing -> error "leaf label is not present in the tree!"

ancestorNodes' :: Eq a => a -> Tree a -> Maybe [a]
ancestorNodes' a (Leaf b) = if a == b then Just [] -- found it! start building a list
                                      else Nothing -- dead end
ancestorNodes' a (Node b l r) =
    if a == b then Just [] -- found it! start building a list, or...
              else ((:) b) <$> (ancestorNodes' a l <|> ancestorNodes' a r) -- look recursively (cf. Alternative typeclass)


-- get a subtree induced by a node with a given label
nodeSubtree :: Eq a => a -> Tree a -> Tree a
nodeSubtree a t
    | not $ isUniqueLabeled t = error "the tree is not labeled uniquely!"
    | otherwise = case nodeSubtree' a t of 
        Just t' -> t'
        Nothing -> error "leaf label is not present in the tree!"

nodeSubtree' :: Eq a => a -> Tree a -> Maybe (Tree a)
nodeSubtree' a (Leaf b) = if a == b then Just (Leaf b) else Nothing
nodeSubtree' a t@(Node b l r) = if a == b then Just t else nodeSubtree' a l <|> nodeSubtree' a r 

-- ** COMPUTE ANCESTRAL CONFIGURATIONS ** 




-- ** HELPER FUNCTIONS **
isListSubset :: Eq a => [a] -> [a] -> Bool
isListSubset xs ys = all (\x -> elem x ys) xs

