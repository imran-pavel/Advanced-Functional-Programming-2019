{-|
Assuming you'll call using a tree that has integer value as key and integer array as value. If you want to call with a Tree that
has some other type as key and value, you'll need to change the types of 'read' function in main at the bottom first (lines 80 and 81) and then call using your
own some other type of tree.

I'm calling using this tree:
Node 20 [1,2,3] (Node 10 [2,2,2] (Node 1 [1, 10] Empty Empty) (Node 15 [8,9] Empty (Node 17 [4, 34] Empty Empty))) (Node 30 [4] Empty Empty)

So after downloading the file run this first: ghc --make Task4-3.hs
then run like any of the following examples to search or insert (I'm running it on windows):

-- Task4-3.exe search 80 Node 20 [1,2,3] (Node 10 [2,2,2] (Node 1 [1, 10] Empty Empty) (Node 15 [8,9] Empty (Node 17 [4, 34] Empty Empty))) (Node 30 [4] Empty Empty)
        -- No key 80 in tree, will return nothing

-- Task4-3.exe insert 80 35 Node 20 [1,2,3] (Node 10 [2,2,2] (Node 1 [1, 10] Empty Empty) (Node 15 [8,9] Empty (Node 17 [4, 34] Empty Empty))) (Node 30 [4] Empty Empty)
        -- insert the key 80 with value 35
    
-- Task4-3.exe search 1 Node 20 [1,2,3] (Node 10 [2,2,2] (Node 1 [1, 10] Empty Empty) (Node 15 [8,9] Empty (Node 17 [4, 34] Empty Empty))) (Node 30 [4] Empty Empty)
        -- searches the key 1 in tree and returns the value array wrapped in Maybe in that node. In this case Just [1, 10]

-- Task4-3.exe insert 10 135 Node 20 [1,2,3] (Node 10 [2,2,2] (Node 1 [1, 10] Empty Empty) (Node 15 [8,9] Empty (Node 17 [4, 34] Empty Empty))) (Node 30 [4] Empty Empty)
        -- inserts 135 where key of node is 10
-}


{-|

Write an interactive program that takes in commands to  insert to a binary tree and search the tree (find a value).
You may work from the book example or a solution to Task 3.3

-}


import System.Environment
import System.Directory
import System.IO
import qualified Data.List as DL


data Tree a b = Empty | Node a [b] (Tree a b) (Tree a b) deriving (Show, Eq, Read)


newNode :: (Ord a, Ord b) => a -> b -> Tree a b
newNode k v = Node k [v] Empty Empty


search :: (Ord a) => a -> Tree a b -> Maybe [b]
search _ Empty = Nothing
search k (Node key values leftSubTree rightSubTree)
    | k > key = search k $ rightSubTree
    | k < key = search k $ leftSubTree
    | otherwise = Just $ values


insert :: (Ord a, Ord b) => a -> b -> Tree a b -> Tree a b
insert k v Empty = newNode k v
insert k v (Node key values leftSubTree rightSubTree)
    | k > key = Node key values leftSubTree (insert k v $ rightSubTree)
    | k < key = Node key values (insert k v $ leftSubTree) rightSubTree
    | otherwise = Node key (v:values) leftSubTree rightSubTree


getStrTree :: [String] -> String
getStrTree (command : k : v : treeParts)
    | command == "search" = DL.intercalate " " (v : treeParts)      -- search will have key and tree as parameters. So v is actually part of the tree. 
    | command == "insert" = DL.intercalate " " treeParts
    | otherwise = error "Invalid Command Given ---> Error From getStrTree"


dispatch :: (Ord a, Ord b, Show a, Show b) => String -> a -> b -> Tree a b -> IO()
dispatch command k v tree
    | command == "insert" = print $ insert k v tree
    | command == "search" = print $ search k tree
    | otherwise = error "Invalid Command Given ---> Error From dispatch"


main = do
        (command : k : v : treeParts) <- getArgs
        let strTree = getStrTree (command : k : v : treeParts)
        let tree = read strTree :: Tree Int Int                         -- Change the types here if you want to use a different type of tree
        dispatch command (read k :: Int) (read v :: Int) tree           -- Also change here if you've changed above