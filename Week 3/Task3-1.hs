-- Call like this: isLeapYear $ Date 2019 5 15
-- Call like this: isValidDate $ Date 2019 2 28 
-- Call like this: getNextDate $ Date 2019 2 28
-- Call like this: dateDistance (Date 2019 2 28) (Date 2019 3 5)


{-|

Re-implement the solutions for Week 1 tasks so that you use record as a data type for dates (define it yourself) and make a module (or modules) from the functions..

-}


module DateFunctions
(
    allDates2019
    , isLeapYear
    , isValidDate
    , getNextDate
    , dateDistance
) where

data Date = Date {
                    year :: Int,
                    month :: Int,
                    day :: Int
                } deriving (Show, Eq)



-- Task 1.1
allDates2019 :: [Date]
allDates2019 = [Date 2019 month day | month <- [1..12], day <- [1..31], (((month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && ( day >= 1 && day <= 31)) || (month == 2 && day >= 1 && day <= 28) || ((month == 4 || month == 6 || month == 9 || month == 11) && (day >= 1 && day <= 30)))]



-- Task 1.2
isLeapYear :: Date -> Bool
isLeapYear date = leapYearOrNot $ year date

leapYearOrNot :: Int -> Bool
leapYearOrNot year = if(mod year 4 == 0) then
    if(mod year 100 == 0) then
        if(mod year 400 == 0) then
            True
        else
            False
    else
        True
 else
    False





-- Task 1.3
allDates :: Int -> [Date]
allDates year = [Date year month day | month <- [1..12], day <- [1..31], (((month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && ( day >= 1 && day <= 31)) || (month == 2 && ((day >= 1 && day <= 28) || (day == 29 && (leapYearOrNot year)))) || ((month == 4 || month == 6 || month == 9 || month == 11) && (day >= 1 && day <= 30)))]

isValidDate :: Date -> Bool
isValidDate date = elem date (allDates $ year date)




-- Task 1.4
getNextDate :: Date -> Date
getNextDate date = Date ny nm nd
                    where (ny, nm, nd) = nextDate (year date, month date, day date)

nextDate :: (Int, Int, Int) -> (Int, Int, Int)
nextDate (year, month, day) = if(isValidDate $ Date year month day) then
    if(month == 2) then
        if(day == 28) then
            if(leapYearOrNot year) then
                (year, month, day+1)
            else
                (year, month+1, 1)
        else
            (year, month, day+1)
    else
        if(month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10) then
            if(day == 31) then
                (year, month+1, 1)
            else
                (year, month, day+1)
        else
            if(month == 4 || month == 6 || month == 9 || month == 11) then
                if(day == 30) then
                    (year, month+1, 1)
                else
                    (year, month, day+1)
            else
                if(month == 12) then
                    if(day == 31) then
                        (year+1, 1, 1)
                    else
                        (year, month, day+1)
                else
                    (-1, -1, -1)     ---- Invalid date input
  else
    (-1, -1, -1)    --- Invalid date input


dateDistance :: Date -> Date -> Int
dateDistance fromDate toDate
                        | fromDate == toDate = 0
                        | otherwise = 1 + (dateDistance (getNextDate fromDate) toDate)