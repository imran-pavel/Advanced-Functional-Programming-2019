{-|

String s1 is a substring of string s2 with gap constraint k, if the characters of s1 appear in s2 in the order of s1, and between them there can be from 0 to k characters. That is, when looking for the next character of s1 in s2, we are allowed to skip over at most k characters.
Write a function which, given strings s1 and s2, finds if s1 is a substring of s2 with gap constraint 1.


To try run like any of these:
-- taskFunction "abc" "axyzbxyzc"
-- taskFunction "abc" "axbyc"
-- taskFunction "abc" "axyzybcxyzaxbycz"

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

taskFunction :: String -> String -> Bool
taskFunction s1 s2 = isGapSubStr s1 s2 1 1 s1