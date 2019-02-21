{-|

Run like any of the followings:
- quickCheck (withMaxSuccess 10000 prop_validNextDate)
- quickCheck (withMaxSuccess 10000 prop_nextDate)

-}


{-|

Write code to Quickcheck test a solution to 1.4. (Calculating the next date). Make at least two properties.

-}

import Test.QuickCheck



-- Task6-1

{-|

prop_nextDate: Get next date, then to check that is is correct 'next date', calling next date from here again and then call previous date from there.
Then check if both the 'next date's are the same.

-}

prop_nextDate :: (Int, Int, Int) -> Bool
prop_nextDate dateTuple
                        | (isValidDate dateTuple) == False = True   -- Ignoring randomly set irrelevant values. For example values like (0, -1, -50) won't make sense
                        | otherwise = (previousDate $ nextDate $ nextDate dateTuple) == (nextDate dateTuple)

{-|

prop_validNextDate: Get next date and check if it is a valid date.

-}
prop_validNextDate :: (Int, Int, Int) -> Bool
prop_validNextDate dateTuple
                            | (isValidDate dateTuple) == False = True   -- Ignoring randomly set irrelevant values. For example values like (0, -1, -50) won't make sense
                            | otherwise = isValidDate $ nextDate dateTuple




-- Task1-4                            
leapYearOrNot :: Int -> Bool
leapYearOrNot year =    if(mod year 4 == 0) then
                            if(mod year 100 == 0) then
                                if(mod year 400 == 0) then
                                    True
                                else
                                    False
                            else
                                True
                        else
                            False



allDates :: Int -> [(Int, Int, Int)]
allDates 0 = []
allDates year = [(year, month, day) | month <- [1..12], day <- [1..31], (((month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && ( day >= 1 && day <= 31)) || (month == 2 && ((day >= 1 && day <= 28) || (day == 29 && (leapYearOrNot year)))) || ((month == 4 || month == 6 || month == 9 || month == 11) && (day >= 1 && day <= 30)))]


increaseMonth :: (Int, Int, Int) -> Bool
increaseMonth (year, month, day)
                                | (isValidDate (year, month, day)) && (((any (month==) [1,3,5,7,8,10,12]) && day == 31) || (any (month==) [4,6,9,11] && day == 30) || (month == 2 && day == 29)) = True
                                | (isValidDate (year, month, day)) && (month == 2 && (leapYearOrNot year) == False && day == 28) = True
                                | otherwise = False


decreaseMonth :: (Int, Int, Int) -> Bool
decreaseMonth (year, month, day)
                                | (isValidDate (year, month, day)) && (month /= 1 && day == 1) = True
                                | otherwise = False



nextDate :: (Int, Int, Int) -> (Int, Int, Int)
nextDate (year, month, day)
                            | (isValidDate (year, month, day)) && year == -1 && month == 12 && day == 31 = (1, 1, 1)
                            | (isValidDate (year, month, day)) && month == 12 && day == 31 = (year+1, 1, 1)
                            | increaseMonth (year, month, day) = (year, month+1, 1)
                            | otherwise = (year, month, day+1)


getPreviousMonthDay :: Int -> Int -> Int
getPreviousMonthDay year month
                                | any (month == ) [1,3,5,7,8,10,12] = 31
                                | month == 2 && (leapYearOrNot year) = 29
                                | month == 2 && ((leapYearOrNot year) == False) = 28
                                | otherwise = 30


previousDate :: (Int, Int, Int) -> (Int, Int, Int)
previousDate (year, month, day)
                            | (isValidDate (year, month, day)) && year == 1 && month == 1 && day == 1 = (-1, 12, 31)
                            | (isValidDate (year, month, day)) && month == 1 && day == 1 = (year-1, 12, 31)
                            | decreaseMonth (year, month, day) = (year, month-1, (getPreviousMonthDay year (month-1)))
                            | otherwise = (year, month, day-1)


isValidDate :: (Int, Int, Int) -> Bool
isValidDate (0, _, _) = False
isValidDate (year, month, day) = elem (year, month, day) (allDates year)