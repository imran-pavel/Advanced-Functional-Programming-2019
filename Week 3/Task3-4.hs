{-|

I've written down 2 trees for testing, testTree1 and testTree2. You can try with your own version of trees as well.

Call like any of the followings:

1. To delete a single value run like this: delete 6 8 testTree1
2. To delete the entire tree of testTree1 try this command: delete 3 3 $ delete 3 3 $ delete 22 22 $ delete 22 22 $ delete 4 4 $ delete 4 4 $ delete 25 25 $ delete 25 25 $ delete 20 20 $ delete 20 20 $ delete 50 50 $ delete 50 50 $ delete 1 1 $ delete 1 1 $ delete 2 2 $ delete 2 2 $ delete 52 52 $ delete 52 52 $ delete 6 6 $ delete 6 8 testTree1
   - Of course you can change the testTree1 and call delete
3. Or may be like this :  delete 'i' 3 $ delete 'i' 4 testTree2

Ouput may seem a bit convoluted (because of record syntax), but it actually works.

-}



{-|

Add deletion to Task 3.3. so that deleting (key. value) pair just deletes the value from the list of values for the key.
If the value to be deleted is the only one in the tree, then the node is deleted.You can check up how nodes are deleted in that case,
from e.g. http://www.algolist.net/Data_structures/Binary_search_tree/Removal

-}

import qualified Data.List as DL

data Tree a b = Empty | Node {
                                key :: a
                                , value :: [b]
                                , leftSubTree :: (Tree a b)
                                , rightSubTree :: (Tree a b)
} deriving (Show, Eq)


testTree1 = Node 6 [6,8] (Node 2 [2,2] (Node 1 [1,1] Empty Empty) (Node 3 [3,3] Empty (Node 4 [4,4] Empty Empty))) (Node 50 [50, 50] (Node 20 [20, 20] Empty (Node 22 [22, 22] Empty (Node 25 [25, 25] Empty Empty))) (Node 52 [52, 52] Empty Empty))
testTree2 = Node 'i' [4,3] (Node 'g' [3,4,5] Empty Empty) (Node 'q' [3,2] Empty Empty)



newNode :: (Ord a, Ord b) => a -> b -> Tree a b
newNode k v = Node k [v] Empty Empty


search :: (Ord a) => a -> Tree a b -> Maybe [b]
search _ Empty = Nothing
search k tree
    | k > (key tree) = search k $ rightSubTree tree
    | k < (key tree) = search k $ leftSubTree tree
    | otherwise = Just $ value tree


insert :: (Ord a, Ord b) => a -> b -> Tree a b -> Tree a b
insert k v Empty = newNode k v
insert k v tree
    | k > (key tree) = Node (key tree) (value tree) (leftSubTree tree) (insert k v $ rightSubTree tree)
    | k < (key tree) = Node (key tree) (value tree) (insert k v $ leftSubTree tree) (rightSubTree tree) 
    | otherwise = Node (key tree) (v:(value tree)) (leftSubTree tree) (rightSubTree tree)


delete _ _ Empty = Empty
delete k v tree
    | k > (key tree) = Node (key tree) (value tree) (leftSubTree tree) (delete k v $ rightSubTree tree)
    | k < (key tree) = Node (key tree) (value tree) (delete k v $ leftSubTree tree) (rightSubTree tree)
    | otherwise = deleteNow k v tree




{-| 

When the following function is reached, tree now definitely has k. May or may not have more than 1 element.

-} 
deleteNow :: (Ord a, Ord b) => a -> b -> Tree a b -> Tree a b
deleteNow k v tree
        | (length $ value tree) > 1 = Node (key tree) (DL.delete v $ value tree) (leftSubTree tree) (rightSubTree tree)     -- If there are more than 1 elements in v, than just call Data.List's delete function on it. If v is there will get deleted, otherwise it will be the same list.
        | otherwise = deleteAndReplaceIfPossible k v tree





{-| 

When the following function is reached, tree definitely has only 1 value in it's value list. If that value is the same as v
then delete and replace will occur.

-} 
deleteAndReplaceIfPossible :: (Ord a, Ord b) => a -> b -> Tree a b -> Tree a b
deleteAndReplaceIfPossible k v tree
    | elem v (value tree) = replaceNow k v tree   
    | otherwise = tree       -- tree has only 1 value in it's value list, but it is not v.



replaceNow :: (Ord a, Ord b) => a -> b -> Tree a b -> Tree a b
replaceNow k v tree
    | (leftSubTree tree) == Empty && (rightSubTree tree) == Empty = Empty                           -- tree has no children
    | (leftSubTree tree) == Empty && (rightSubTree tree) /= Empty = rightSubTree tree               -- tree has 1 child
    | (leftSubTree tree) /= Empty && (rightSubTree tree) == Empty = leftSubTree tree                -- tree has 1 child
    | otherwise = Node newKey newValues (leftSubTree tree) newRightSubTree                          -- tree has 2 children
                where
                    (newKey, newValues) = getSmallestKVPair $ rightSubTree tree                     -- Get smallest key and it's values from the right subtree
                    newRightSubTree = deleteRegardlessOfVListLength newKey $ rightSubTree tree      -- Delete the node with the smallest key from the right subtree




getSmallestKVPair :: (Ord a, Ord b) => Tree a b -> (a, [b])
getSmallestKVPair tree
    | (leftSubTree tree) == Empty = ((key tree), (value tree))
    | otherwise = getSmallestKVPair (leftSubTree tree)


deleteRegardlessOfVListLength :: (Ord a, Ord b) => a -> Tree a b -> Tree a b
deleteRegardlessOfVListLength k tree
    | k > (key tree) = Node (key tree) (value tree) (leftSubTree tree) (deleteRegardlessOfVListLength k $ rightSubTree tree)     -- Never gonna happen
    | k < (key tree) = Node (key tree) (value tree) (deleteRegardlessOfVListLength k $ leftSubTree tree) (rightSubTree tree)
    | otherwise = rightSubTree tree         -- Taking whatever is there in right subtree. Left is definitely empty because k here is the k returned from getSmallestKVPair ( parameter of deleteRegardlessOfVListLength in replaceNow ).
