{-

The support of a string x in the list of strings xs is the number of strings in xs where x is a substring.
Given a list of strings xs, and integer k, find  k such two-character strings that have the biggest support in xs
(there can be many equal answers). Return the answer as a list of (support,string) pairs.

E.g. if xs = ["hello","there","all"] and k = 3, then one possible answer is [(2,"ll"),(2,"he"),(1,"th")]


To run try like this:
-- taskFunction ["elephant", "phone", "stone", "tone", "ant", "phil", "soprano", "phoebe"] 2

-}

import qualified Data.List as DL

getAllPossibleListsOfLengthN :: (Eq a) => [a] -> Int -> Int -> [a] -> [a] -> [[a]]

getAllPossibleListsOfLengthN [] currentN initialN generatedList givenList
    | currentN == initialN = []  
    | otherwise = getAllPossibleListsOfLengthN givenList currentN currentN generatedList givenList 

getAllPossibleListsOfLengthN (x:xs) 0 _ generatedList _ = [generatedList]
getAllPossibleListsOfLengthN (x:xs) currentN initialN generatedList givenList =
    DL.nub $ takeX ++ ignoreX
    where
        takeX = getAllPossibleListsOfLengthN xs (currentN-1) initialN (x:generatedList) givenList
        ignoreX = getAllPossibleListsOfLengthN xs currentN initialN generatedList givenList



getAllCandidateStrings xs = allCandidateStrings
    where
        allCandidateStrings = DL.nub $ foldl foldFunction2 [] xs
        foldFunction2 = (\acc inputString -> acc ++ (candidateStrings inputString))
        candidateStrings = (\inputString -> getAllPossibleListsOfLengthN inputString 2 2 [] inputString)

countSubStrTo :: String -> [String] -> Int
countSubStrTo x xs = length $ filter filteringFunction xs
    where filteringFunction = (\str -> DL.isInfixOf x str)


getSupportStringPairs :: [String] -> [(Int, String)]
getSupportStringPairs xs = foldl foldfunction [] allCandidateStrings
    where
        allCandidateStrings = getAllCandidateStrings xs
        foldfunction = (\acc candidateString -> ((countSubStrTo candidateString xs), candidateString) : acc)


taskFunction :: [String] -> Int -> [(Int, String)]
taskFunction xs k = take k $ reverse $ DL.sortBy (\(sprt1, _) (sprt2, _) -> compare sprt1 sprt2) supportStringTuples
    where
        supportStringTuples = getSupportStringPairs xs
        
