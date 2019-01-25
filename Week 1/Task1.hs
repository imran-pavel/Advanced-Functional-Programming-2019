{-|

Write a list comprehension expression that makes a list of all dates of year 2019 using tuples (year, month, day), so the first one is
(2019,1,1) and the last is (2019,12,31)

-}

allDates2019 = [(2019, month, day) | month <- [1..12], day <- [1..31], (((month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && ( day >= 1 && day <= 31)) || (month == 2 && day >= 1 && day <= 28) || ((month == 4 || month == 6 || month == 9 || month == 11) && (day >= 1 && day <= 30)))]