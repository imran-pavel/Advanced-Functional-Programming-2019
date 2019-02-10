{-|
The book example takes commands as command line parameters. I hope this is what was meant by "interactive program".

First create two text files: "tree.txt" and "output.txt"

Write your tree in "tree.txt" file, "output.txt" file can be empty. I am not reading anythig from "output.txt".
For testing I wrote this tree in "tree.txt" file (Int key and Int array as values):

Node 20 [1,2,3] (Node 10 [2,2,2] (Node 1 [1, 10] Empty Empty) (Node 15 [8,9] Empty (Node 17 [4, 34] Empty Empty))) (Node 30 [4] Empty Empty)


Assuming you'll call using a tree that has integer value as key and integer array as value. If you want to call with a Tree that
has some other type as key and value, you'll need to change the types of Tree/read in lines 75, 80, 87, 98 and then write down your tree in "tree.txt" file.

After downloading the file run this first (keep the .hs and the 2 .txt files in the same directory): ghc --make Task4-4.hs
then run like any of the following examples to search or insert (I'm running it on windows):

-- Task4-4.exe search 17
        -- returns "Just [4,34]" for the above tree in output.txt file

-- Task4-4.exe search 13
        -- returns "Nothing" for the above tree in output.txt file
    
-- Task4-4.exe insert 13 39
        -- Tree gets updated and updated tree is in tree.txt file

-- Task4-4.exe search 10
        -- returns "Just [2,2,2]" for the above tree in output.txt file
-}


{-|

Improve 4.3. with commands to read a tree from a file and write it to a file.

-}



import System.Environment
import System.Directory
import System.IO
import Control.Monad
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



dispatch :: [String] -> Tree Int Int -> IO()                                    -- Change the Tree types here if you want to use a different type of tree
dispatch (command:xs) tree
        | command == "insert" = do
                                        let (strKey : strValue : []) = xs
                                        (tempName, tempHandle) <- openTempFile "." "temp"
                                        hPutStr tempHandle (show (insert (read strKey :: Int) (read strValue :: Int) tree))   -- Change return types of read here
                                        hClose tempHandle
                                        removeFile "tree.txt"
                                        renameFile tempName "tree.txt"
        | command == "search" = do
                                        let (strKey : []) = xs
                                        (tempName, tempHandle) <- openTempFile "." "temp"
                                        hPutStr tempHandle (show (search (read strKey :: Int) tree))        -- Change return types of read here
                                        hClose tempHandle
                                        removeFile "output.txt"
                                        renameFile tempName "output.txt"
        | otherwise = error "Invalid Command Given ---> Error From dispatch"



main = do
        arguments <- getArgs
        strTree <- readFile "tree.txt"
        let tree = read strTree :: Tree Int Int                         -- Change the types here if you want to use a different type of tree
        dispatch arguments tree                                        
