{-

Make a function that, given a list xs, a number n, and a function f, constructs a list ys such that: 

ys has n elements,
ys is constructed of elements of xs, and
f evaluates to true on ys.
If xs = "noway", n = 2, and f always returns true (e.g. f _ = True), then we get the same list as in the example for Task 2.2.

Edit:
(1) It is acceptable to apply f on the whole list (thereby either rejecting the answer or accepting it,
    maybe return [] in this case) or on each individual element (thereby rejecting or accepting each element of the list).
(2) You may either generate a list of length n of elements of xs, or a list of lists of length n.


To run try like any of these:
-- taskFunction [1,2,3] 2 (\ _ -> True)
-- taskFunction [1,2] 3 (\ _ -> True)
-- taskFunction "abc" 2 (\ (x:_) -> x == 'b')


Comment 1:
Meaning, one pass of traversing givenList is complete. But no elements were taken, only ignored. Continuing will end up in infinite loop.

Comment 2:
Meaning, something was taken while traversing. [] indicates we've traversed the given list once. But there is still more to take,
since currentN is not. So start again with the given list and change initalN.
-}

import qualified Data.List as DL

getAllPossibleListsOfLengthN :: (Eq a) => [a] -> Int -> Int -> [a] -> [a] -> [[a]]

getAllPossibleListsOfLengthN [] currentN initialN generatedList givenList
    | currentN == initialN = []   -- Comment 1. Check Above.
    | otherwise = getAllPossibleListsOfLengthN givenList currentN currentN generatedList givenList  -- Comment 2. Check Above.

getAllPossibleListsOfLengthN (x:xs) 0 _ generatedList _ = [generatedList]
getAllPossibleListsOfLengthN (x:xs) currentN initialN generatedList givenList =
    DL.nub $ takeX ++ ignoreX
    where
        takeX = getAllPossibleListsOfLengthN xs (currentN-1) initialN (x:generatedList) givenList
        ignoreX = getAllPossibleListsOfLengthN xs currentN initialN generatedList givenList


taskFunction :: (Eq a) => [a] -> Int -> ([a] -> Bool) -> [[a]]
taskFunction xs n f = filter f candidateList
    where
        candidateList = getAllPossibleListsOfLengthN xs n n [] xs