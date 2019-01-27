{-|

The support of a string x in the list of strings xs is the number of strings in xs where x is a substring.
Given a list of strings xs, and integer k, find  k such two-character strings that have the biggest support in 
xs (there can be many equal answers). Return the answer as a list of (support,string) pairs.

E.g. if xs = ["hello","there","all"] and k = 3, then one possible answer is [(2,"ll"),(2,"he"),(1,"th")]

-}



{-|

Call like this:   getKCorrectSupportTuples ["hello", "there", "all"] 3

-}




import qualified Data.List as DL



ignoreF _ = True


getAllPossibleNCharacterStrings :: (Eq a) => [a] -> Int -> [a] -> [a] -> ([[a]] -> Bool) -> Int -> [a] -> [[a]]
getAllPossibleNCharacterStrings _ 0 ys _ _ n _
                                            | length ys == n = [ys]
                                            | otherwise = []


getAllPossibleNCharacterStrings [] remainingN ys initialYs f n givenList
                                                        | ys == initialYs = []            
                                                        | otherwise = getAllPossibleNCharacterStrings givenList remainingN ys ys f n givenList       


getAllPossibleNCharacterStrings (x:xs) remainingN ys initialYs f n givenList = DL.nub $ (checkWithF f takeX) ++ (checkWithF f ignoreX)
                                                                                where
                                                                                    takeX = getAllPossibleNCharacterStrings xs (remainingN-1) (x:ys) initialYs f n givenList
                                                                                    ignoreX = getAllPossibleNCharacterStrings xs remainingN ys initialYs f n givenList




checkWithF :: ([a] -> Bool) -> [a] -> [a]
checkWithF f currentList
                        | f currentList = currentList
                        | otherwise = []



allPossibleNCharacterCandidateStringsOfXs :: (Eq a) => [[a]] -> Int -> [[a]]
allPossibleNCharacterCandidateStringsOfXs xs n = DL.nub $ DL.intercalate [] $ map getAllCandidateStringsForThisString xs
                                                    where getAllCandidateStringsForThisString = (\currentString -> getAllPossibleNCharacterStrings currentString n [] [] ignoreF n currentString) 



calculateSupport :: (Eq a) => [a] -> [[a]] -> Int        
calculateSupport candidateString givenStrings = foldl foldFunction 0 givenStrings
                                            where foldFunction = (\acc currentString -> if (DL.isInfixOf candidateString currentString) then 
                                                                                            acc+1
                                                                                        else acc
                                                                )



getSupportTuples :: (Eq a) => [[a]] -> [[a]] -> [(Int, [a])]                                                               
getSupportTuples candidateStrings givenStrings = map mapFunction candidateStrings
                                                    where mapFunction = (\currentCandidateString -> ((calculateSupport currentCandidateString givenStrings), currentCandidateString))





sortSupportTuples :: [(Int, [a])] -> [(Int, [a])]
sortSupportTuples supportTuples = reverse $ DL.sortBy (\(sprt1, _) (sprt2, _) -> compare sprt1 sprt2) supportTuples



getKCorrectSupportTuples :: (Eq a) => [[a]] -> Int -> [(Int, [a])]
getKCorrectSupportTuples givenStrings k = take k $ sortSupportTuples chosenTuples
                                            where
                                                allPossibleCandidates = allPossibleNCharacterCandidateStringsOfXs givenStrings 2
                                                chosenTuples = getSupportTuples allPossibleCandidates givenStrings
