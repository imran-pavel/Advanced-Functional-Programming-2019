{-
Task 1.1
Write a list comprehension expression that makes a list of all dates of year 2019 using tuples (year, month, day), so the first one is
(2019,1,1) and the last is (2019,12,31)

Solution:
Call like this in ghci: allDates2019

-}

allDates2019 :: [(Int, Int, Int)]
allDates2019 = allDates 2019


allDates :: Int -> [(Int, Int, Int)]
allDates year = [(year, month, day) | month <- [1..12], day <- [1..31], isValidDate (year, month, day)]
------------------------------------------------------------------------------------------------------------
{-

Task 1.2
Write a function to check if a year is a leap year.

Solution:
Call like this in ghci: leapYearOrNot 1954

-}

leapYearOrNot :: Int -> Bool
leapYearOrNot year
    | (mod year 4) /= 0 = False
    | (mod year 100) /= 0 = True
    | (mod year 400) /= 0 = False
    | otherwise = True
------------------------------------------------------------------------------------------------------------
{-

Task 1.3
Write a function to check if a given date (y,m,d) is correct.

Solution:
Call like this in ghci: isValidDate (2019, 2, 32)

-}

isValidDate :: (Int, Int, Int) -> Bool
isValidDate (year, month, day)
    | year == 0 = False
    | (elem month [1,3,5,7,8,10,12]) && (day >= 1 && day <= 31) = True
    | (elem month [4,6,9,11]) && (day >= 1 && day <= 30) = True
    | (elem month [2]) && (day == 29) && (leapYearOrNot year) = True
    | (elem month [2]) && (day >= 1 && day <= 28) = True
    | otherwise = False
------------------------------------------------------------------------------------------------------------
{-

Task 1.4
Write a function that, given a date, calculates the next date.

Using that function, write another function that tells the distance of two dates.
The distance of today and tomorrow is 1, today and the day after tomorrow is 2, etc.
Use recursively the function that calculates the next date.


Solution:
Call like this in ghci: dateDistance (2019, 1, 1) (2019, 12, 31)
-}

nextDate :: (Int, Int, Int) -> (Int, Int, Int)
nextDate (year, month, day)
    | (isValidDate (year, month, day)) == False = (-1, -1, -1)
    | (year == -1) && (month == 12) && (day == 31) = (1, 1, 1)
    | (month == 12) && (day == 31) = (year+1, 1, 1)
    | ((elem month [1,3,5,7,8,10]) && (day == 31)) || ((elem month [4,6,9,11]) && (day == 30)) || ((elem month [2]) && (day == 28)) = (year, month+1, 1)
    |  otherwise = (year, month, day+1)


calculateDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int
calculateDistance (fromYear, fromMonth, fromDay) (toYear, toMonth, toDay) distance
    | (fromYear == toYear) && (fromMonth == toMonth) && (fromDay == toDay) = distance
    | otherwise = calculateDistance (nextDate (fromYear, fromMonth, fromDay)) (toYear, toMonth, toDay) (distance+1)
    

dateDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
dateDistance fromDate toDate
    | ((isValidDate fromDate) == False) || ((isValidDate toDate) == False) = -1
    | otherwise = calculateDistance fromDate toDate 0
------------------------------------------------------------------------------------------------------------