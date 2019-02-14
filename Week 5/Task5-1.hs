{-|

To test just run the applicativeTriangle function.

-}


{-|

The book used list comprehension to answer the following: Which right triangle that has integers for all sides and all sides equal to or 
smaller than 10 has a perimeter of 24?

Do the same using the fact that list is an applicative functor, using the style of the applicatives as proposed in the book.

-}


import qualified Data.List as DL


getTriangle :: Int -> Int -> Int -> [Int]
getTriangle = (\a b c ->    if ((a*a == b*b + c*c) && (a + b + c == 24) && (a > b && b > c)) then
                                [a, b, c]
                            else
                                []
                        )


applicativeTriangle :: [Int]
applicativeTriangle = DL.intercalate [] $ pure getTriangle <*> [1..10] <*> [1..10] <*> [1..10]