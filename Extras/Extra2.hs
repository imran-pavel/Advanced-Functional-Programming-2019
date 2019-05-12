{-|

Find a function that, given a string x and a gap constraint k, finds all such strings y that y is a substring of x with gap constraint k.
Gap constraint is defined in Extra1.

Try running like this: taskFunction "abcde" 2

-}


isGapSubStr :: String -> String -> Int -> Int -> String -> Bool
isGapSubStr [] [] _ _ _ = True
isGapSubStr xs [] _ _ _ = False
isGapSubStr [] ys _ _ _ = True

isGapSubStr (x:xs) (y:ys) 0 givenK wholeS1
    | x /= y = isGapSubStr wholeS1 (y:ys) givenK givenK wholeS1
    | otherwise = isGapSubStr xs ys givenK givenK wholeS1
    
isGapSubStr (x:xs) (y:ys) currentK givenK wholeS1
    | x == y = isGapSubStr xs ys givenK givenK wholeS1
    | otherwise = isGapSubStr (x:xs) ys (currentK-1) givenK wholeS1


getAllPossibleStrings :: String -> String -> [String]
getAllPossibleStrings [] generatedString = [generatedString]
getAllPossibleStrings (x:xs) generatedString = takeX ++ ignoreX
    where
        takeX = getAllPossibleStrings xs (generatedString ++ [x])
        ignoreX = getAllPossibleStrings xs generatedString


taskFunction :: String -> Int -> [String]
taskFunction x k = filter filteringFunction candidateStrings
    where
        candidateStrings = getAllPossibleStrings x []
        filteringFunction = (\candidateString-> isGapSubStr candidateString x k k candidateString)
