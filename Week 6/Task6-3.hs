{-|

Steps to run:
- First do : ghc --make Task6-3.hs
- Then do : Task6-3.exe 10 4 3 + 2 * -
- I am running it on Windows

It can be Task6-3.exe ANY_VALID_RPN_SEPARATED_WITH_SPACES. Acceptable operators are: +, -, *, /, ^, ln and sum.

-}


{-|

Make a version of the Reverse Polish notation calculator that uses the Writer monad to make a log of the operations.

-}


import System.Environment
import System.IO
import qualified Control.Monad.Writer as CMW
import qualified Data.List as DL

type RPNotation = [String]
type Stack = [Double]


rpnWMonad :: (RPNotation, Stack) -> CMW.Writer [String] (RPNotation, Stack)
rpnWMonad ([], x:[]) = CMW.writer (([], x:[]), ["Result is " ++ (show x)])
rpnWMonad (("+":xs), (x:y:ys)) = CMW.writer ((xs, ((x+y):ys)), ["Adding " ++ (show x) ++ " and " ++ (show y)]) >>= rpnWMonad
rpnWMonad (("-":xs), (x:y:ys)) = CMW.writer ((xs, ((y-x):ys)), ["Subtracting " ++ (show y) ++ " and " ++ (show x)]) >>= rpnWMonad
rpnWMonad (("*":xs), (x:y:ys)) = CMW.writer ((xs, ((x*y):ys)), ["Multiplying " ++ (show x) ++ " and " ++ (show y)]) >>= rpnWMonad
rpnWMonad (("/":xs), (x:y:ys)) = CMW.writer ((xs, ((y/x):ys)), ["Dividing " ++ (show y) ++ " and " ++ (show x)]) >>= rpnWMonad
rpnWMonad (("^":xs), (x:y:ys)) = CMW.writer ((xs, ((y**x):ys)), ["Powering " ++ (show y) ++ " upto " ++ (show x)]) >>= rpnWMonad
rpnWMonad (("ln":xs), (y:ys)) = CMW.writer ((xs, ((log y):ys)), ["Doing ln of " ++ (show y)]) >>= rpnWMonad
rpnWMonad (("sum":xs), ys) = CMW.writer ((xs, [sum ys]), ["Summing the list " ++ (show ys)]) >>= rpnWMonad
rpnWMonad ((x:xs), stack) = CMW.writer ((xs, ((read x :: Double):stack)), ["Pushing number " ++ x]) >>= rpnWMonad


main = do
            rpNotation <- getArgs
            putStrLn ""
            putStr "Given Notation: "
            print rpNotation
            let resWriter = CMW.runWriter $ rpnWMonad (rpNotation, [])
            let output = unlines $ snd $ resWriter
            putStrLn output