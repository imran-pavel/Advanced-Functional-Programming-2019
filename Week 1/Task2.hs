{-|

Write a function to check if a year is a leap year.

-}


leapYearOrNot year = if(mod year 4 == 0) then
                        if(mod year 100 == 0) then
                            if(mod year 400 == 0) then
                                "Yes, leap year"
                            else
                                "Not a leap year"
                        else
                            "Yes, leap year"
                     else
                        "Not a leap year"