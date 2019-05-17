{-|

After loading run the main function. Then enter one number/operator at a time of a valid reverse polish notation.
For example, if you want to evaluate this notation: 10 4 3 + 2 * -
After running main, type in 10 and hit enter, then 4 and so on.

At any moment to stop type in "quit" and hit enter.


-}



{-|

Make an executable version of the Reverse Polish notation calculator (a program that reads the tokens from standard input and after each 
operation writes the top of the stack to standard output, thus imitating a display.

-}

import Control.Monad


calculate :: [Double] -> String -> [Double]
calculate (x:y:ys) "+" = (x+y):ys
calculate (x:y:ys) "-" = (y-x):ys
calculate (x:y:ys) "*" = (x*y):ys
calculate (x:y:ys) "/" = (y/x):ys
calculate (x:y:ys) "^" = (y ** x):ys
calculate (x:xs) "ln" = (log x):xs
calculate xs "sum" = [sum xs]
calculate xs number = (read number :: Double) : xs



runCalculator :: [Double] -> IO()
runCalculator stack = do
                            input <- getLine
                            when (input /= "quit") $ do
                                                        let newStack = calculate stack input
                                                        print newStack
                                                        runCalculator newStack
                            return ()
                                
                            

main = do
            print "Please enter a valid polish notation one number/operator at a time."
            runCalculator []
