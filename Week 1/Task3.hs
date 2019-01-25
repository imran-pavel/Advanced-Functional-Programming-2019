{-|

Write a function to check if a given date (y,m,d) is correct.

Leap year examples: 2204, 1688
Non leap year examples: 1734, 2200

-}



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



allDates year = [(year, month, day) | month <- [1..12], day <- [1..31], (((month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && ( day >= 1 && day <= 31)) || (month == 2 && ((day >= 1 && day <= 28) || (day == 29 && (leapYearOrNot year)))) || ((month == 4 || month == 6 || month == 9 || month == 11) && (day >= 1 && day <= 30)))]


isValidDate (year, month, day) = elem (year, month, day) (allDates year)