{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tree where

import Data.List ( nub )
import Control.Applicative ( Alternative((<|>)) )

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Eq

-- ** PRINT in a visually digestible format ** 
instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Node a l r) =
        case (l,r) of (Leaf x, Leaf y) -> stripExtraQuotes (show a) ++ "\n"
                                                   ++ "|---" ++ stripExtraQuotes (show x) ++ "\n|\n"
                                                   ++ "|---" ++ stripExtraQuotes (show y)
                      (Node a' l' r', Leaf x) -> stripExtraQuotes (show a) ++ "\n"
                                                          ++ decorateSubtreeString' n (show (Node a' l' r'))
                                                          ++ "|\n|---" ++ stripExtraQuotes (show x)
                      (Leaf x, Node a' l' r') -> stripExtraQuotes (show a) ++ "\n"
                                                          ++ "|---" ++ stripExtraQuotes (show x) ++ "\n|\n"
                                                          ++ decorateSubtreeString n (show (Node a' l' r'))
                      (Node a' l' r', Node a'' l'' r'') -> stripExtraQuotes (show a) ++ "\n"
                                                                    ++ decorateSubtreeString' n (show (Node a' l' r')) ++"|\n"
                                                                    ++ decorateSubtreeString n (show (Node a'' l'' r''))
        where n = length $ stripExtraQuotes $ show a

stripExtraQuotes :: String -> String
stripExtraQuotes = filter (/= '"')

decorateSubtreeString :: Int -> String -> String
decorateSubtreeString n s = unlines $ dec $ lines s where dec (x:xs) = ("|---" ++ replicate n '-' ++ x ) : map (\ln -> "    " ++ replicate n ' ' ++ ln) xs

decorateSubtreeString' :: Int -> String -> String
decorateSubtreeString' n s = unlines $ dec $ lines s where dec (x:xs) = ("|---" ++ replicate n '-' ++ x ) : map (\ln -> "|   " ++ replicate n ' ' ++ ln) xs

-- ** BASIC FUNCTIONS ** 

-- return the tuple of subtrees
subtrees :: Tree a -> Maybe (Tree a, Tree a)
subtrees (Leaf _) = Nothing
subtrees (Node _ l r) = Just (l, r)

left :: Tree a -> Maybe (Tree a)
left t = fst <$> subtrees t

right :: Tree a -> Maybe (Tree a)
right t = snd <$> subtrees t

label :: Tree a -> a
label (Leaf a) = a
label (Node a _ _) = a

-- get labels of all nodes in a list
nodeLabels :: Tree a -> [a]
nodeLabels (Leaf a) = [a]
nodeLabels (Node a l r) = nodeLabels l ++ [a] ++ nodeLabels r

-- get all leaf labels
leafLabels :: Tree a -> [a]
leafLabels (Leaf a) = [a]
leafLabels (Node _ l r) = leafLabels l ++ leafLabels r

-- get labels of all internal nodes
internalLabels :: Tree a -> [a]
internalLabels (Leaf _) = []
internalLabels (Node a l r) = internalLabels l ++ [a] ++ internalLabels r

-- predicate to check if the nodes of a tree are labeled uniquely
isUniqueLabeled :: Eq a => Tree a -> Bool
isUniqueLabeled t = l == nub l where l =  nodeLabels t

-- compute the number of nodes in the tree
numNodes :: Tree a -> Int
numNodes (Leaf _) = 1
numNodes (Node _ l r) = 1 + numNodes l + numNodes r

-- compute the number of leaves in the tree
numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node _ l r) = numLeaves l + numLeaves r

-- get most recent common ancestor (MRCA) of a given list of leaf (labels)
mrcaNode :: Eq a => Tree a -> [a] -> Tree a
mrcaNode t ls
    | not $ isUniqueLabeled t = error "the tree is not labeled uniquely!"
    | otherwise = case mrcaNode' ls t of
        Just t' -> t'
        Nothing -> error "leaf labels do not match the tree!"


mrcaNode' :: Eq a => [a] -> Tree a -> Maybe (Tree a)
mrcaNode' [l] (Leaf l') = if l == l' then Just (Leaf l) else Nothing -- first match on leaves, which happens only if the list has a single element
mrcaNode' _   (Leaf _)  = Nothing                                    -- bigger lists never match
mrcaNode' ls t@(Node _ l r) = if isListSubset ls $ nodeLabels t   -- if the labels in the list are in subtree, then we are on the right track, we only need to ensure minimality
                              then case (isListSubset ls $ nodeLabels l, isListSubset ls $ nodeLabels r) of
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
              else (b :) <$> (ancestorNodes' a l <|> ancestorNodes' a r) -- look recursively (cf. Alternative typeclass)


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

-- ** HELPER FUNCTIONS **

-- predicate to determine if xs is a subset of ys
isListSubset :: Eq a => [a] -> [a] -> Bool
isListSubset xs ys = all (`elem` ys) xs

-- predicate to determine if the second list contains any element from the first list
containsAnyElemFromList :: Eq a => [a] -> [a] -> Bool
containsAnyElemFromList [] _ = False
containsAnyElemFromList _ [] = False
containsAnyElemFromList (x:xs) ys = x `elem` ys || containsAnyElemFromList xs ys