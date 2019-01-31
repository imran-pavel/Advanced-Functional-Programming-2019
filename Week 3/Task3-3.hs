{-|

I've written down 2 trees for testing, testTree1 and testTree2. You can try with your own version of trees as well.

Call like any of the followings:
1. search 'z' testTree2  
2. search 'z' $ insert 'z' 50 testTree2

or may be like this:
1. search 100 testTree1
2. search 100 $ insert 100 20 $ insert 100 12 testTree1

-}



{-|

Create a binary tree type that stores (key, [value]) pairs. Inserting with existing key appends to the key's list and search would give the whole
list as an answer, if an answer is found. Return a Maybe value when searching. (Just value if found, Nothing otherwise).

-}


data Tree a b = Empty | Node {
                                key :: a
                                , value :: [b]
                                , leftSubTree :: (Tree a b)
                                , rightSubTree :: (Tree a b)
} deriving (Show, Eq)


testTree1 = Node 6 [1,2,3] (Node 2 [2,2,2] (Node 1 [1, 10] Empty Empty) (Node 3 [3,2,3] Empty (Node 4 [4,4,4] Empty Empty))) Empty
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