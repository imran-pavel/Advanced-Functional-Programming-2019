
{-|

Write a function that, given a date, calculates the next date.

Using that function, write another function that tells the distance of two dates. The distance of today and tomorrow is 1, today and the day after tomorrow is 2, etc. Use recursively the function that calculates the next date.


To run this solution, call "dateDistance" function like this: dateDistance (2019, 1, 7) (2019, 1, 12)
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





nextDate (year, month, day) = if(isValidDate (year, month, day)) then
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






allDates year = [(year, month, day) | month <- [1..12], day <- [1..31], (((month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && ( day >= 1 && day <= 31)) || (month == 2 && ((day >= 1 && day <= 28) || (day == 29 && (leapYearOrNot year)))) || ((month == 4 || month == 6 || month == 9 || month == 11) && (day >= 1 && day <= 30)))]


isValidDate (year, month, day) = elem (year, month, day) (allDates year)





dateDistance fromDate toDate
                            | fromDate == toDate = 0
                            | otherwise = 1 + (dateDistance (nextDate fromDate) toDate)