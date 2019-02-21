{-|


Run like any of these:
- insert 40 26 testTree
- insert 't' "The Martian" testTree2

You can try with your own custom tree.

-}


{-|

Task 3.3 using DIffList for storing the lists.

-}

import qualified Data.List as DL


data Tree a b = Empty | Node a (DiffList b) (Tree a b) (Tree a b) deriving (Show)
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

testTree = insert 40 18 $ insert 50 15 $ insert 35 2 $ insert 35 18 $ insert 25 15 $ insert 50 2 Empty
testTree2 = insert 'k' "Kingdom of Heaven" $ insert 'p' "Prometheus" $ insert 'b' "Body of Lies" $ insert 'a' "Alien" $ insert 'e' "Exodus" $ insert 'a' "American Gangster" Empty

instance (Show a) => Show (DiffList a) where
    show (DiffList f) = show $ fromDiffList $ DiffList f


toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []


newNode :: (Ord a, Ord b) => a -> b -> Tree a b
newNode k v = Node k (toDiffList [v]) Empty Empty


search :: (Ord a) => a -> Tree a b -> Maybe [b]
search _ Empty = Nothing
search k (Node key values leftSubTree rightSubTree)
    | k > key = search k $ rightSubTree
    | k < key = search k $ leftSubTree
    | otherwise = Just $ fromDiffList $ values


insert :: (Ord a, Ord b) => a -> b -> Tree a b -> Tree a b
insert k v Empty = newNode k v
insert k v (Node key values leftSubTree rightSubTree)
    | k > key = Node key values leftSubTree (insert k v $ rightSubTree)
    | k < key = Node key values (insert k v $ leftSubTree) rightSubTree
    | otherwise = Node key (toDiffList $ v:(fromDiffList values)) leftSubTree rightSubTree

