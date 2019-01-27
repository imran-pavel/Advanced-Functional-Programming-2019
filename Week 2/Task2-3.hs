{-|

Make a function that, given a list xs, a number n, and a function f, constructs a list ys such that: 
ys has n elements,
ys is constructed of elements of xs, and
f evaluates to true on ys.
If xs = "noway", k = 2, and f always returns true (e.g. f _ = True), then we get the same list as in the example for Task 2.2.

-}



{-|
    Call like this:       getYs "noway" 2 testFunction
    or may be like this   getYs [1,2,3] 2 testFunction

    You can give some other list and function as well.
-}


{-|

How it works:
Traverse the given list. At each item either create a list taking the item and create another ignoring the item. Keep count when moving to the
next item.
Once n items taken, treat "ys" as a new separate item (making it a list like this [ys]).
If "givenList" is exhausted but "ys" still doesn't have n items, then check cases in Comment 1 and 2 and continue.


Comment 1:
Meaning a complete pass of traversing "givenList" has been completed without any addition of element in "ys".
This happens when solutionFunction goes to "ignoreX" only when making choice.
So after one pass if there is no update in "ys" then stop.


Comment 2:
One pass of traversing the "givenList" is complete, "ys" has some new elements but still does not have n elements.
So, start another pass from the beginning of the "givenList" but this time update "initalYs" with "ys".

-}



import qualified Data.List as DL



testFunction _ = True


solutionFunction :: (Eq a) => [a] -> Int -> [a] -> [a] -> ([[a]] -> Bool) -> Int -> [a] -> [[a]]
solutionFunction _ 0 ys _ _ n _
                            | length ys == n = [ys]
                            | otherwise = []


solutionFunction [] remainingN ys initialYs f n givenList
                                                        | ys == initialYs = []              -- Comment 1
                                                        | otherwise = solutionFunction givenList remainingN ys ys f n givenList             -- Comment 2


solutionFunction (x:xs) remainingN ys initialYs f n givenList = DL.nub $ (checkWithF f takeX) ++ (checkWithF f ignoreX)
                                                        where
                                                            takeX = solutionFunction xs (remainingN-1) (x:ys) initialYs f n givenList
                                                            ignoreX = solutionFunction xs remainingN ys initialYs f n givenList



checkWithF f currentList
                        | f currentList = currentList
                        | otherwise = []



getYs xs n f = solutionFunction xs n [] [] f n xs

