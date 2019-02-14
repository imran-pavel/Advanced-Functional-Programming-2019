{-|

Create 3 text files in the same directory as this file. Put some text in any of those 2 files.
Now load this file and then run the main function.

Enter the name of 2 files having some texts one after another and the third file name should be the empty text file. After you enter the last one it will
write in the last file.

If you enter some file name that is not available it will run again.

-}



{-|

Write a program that asks names for three files, and then reads the contents of the first and second file, sorts the resulting lines,
and writes the sorted contents into the third file. Manage IO Exceptions.

-}

import System.Environment
import Control.Exception
import System.IO
import System.Directory
import Control.Monad
import qualified Data.List as DL


removeAndRename :: String -> String -> Handle -> IO ()
removeAndRename fileName tempFileName tempHandle = do
                                                    removeFile fileName
                                                    renameFile tempFileName fileName
                                                    putStrLn "Write Successful"



main = do
            putStrLn "Enter first file name: "
            file1 <- getLine
            putStrLn "Enter second file name: "
            file2 <- getLine
            putStrLn "Enter third file name: "
            file3 <- getLine


            file1Contents <- catch (readFile file1) (\err -> do
                                                                print $ show (err :: IOException)
                                                                return $ "Read Failed")
            when (file1Contents == "Read Failed") $ do
                                                        putStrLn "Read Failed. Try Again."
                                                        main



            file2Contents <- catch (readFile file2) (\err -> do
                                                                print $ show (err :: IOException)
                                                                return $ "Read Failed")
            when (file2Contents == "Read Failed" && file1Contents /= "Read Failed") $ do
                                                                                        putStrLn "Read Failed. Try Again."
                                                                                        main


            when (file2Contents /= "Read Failed" && file1Contents /= "Read Failed") $ do 
                                                                                        let allLines = unlines $ DL.sort $ (lines file1Contents) ++ (lines file2Contents)
                                                                                        (tempName, tempHandle) <- openTempFile "." "temp"
                                                                                        hPutStr tempHandle allLines
                                                                                        hClose tempHandle
                                                                                        catch (removeAndRename file3 tempName tempHandle) (\e -> do
                                                                                                                                                    let errorMessage = show (e :: IOException)
                                                                                                                                                    putStrLn $ file3 ++ " couldn't be written to. Try Again"
                                                                                                                                                    removeFile tempName
                                                                                                                                                    main
                                                                                                                                        )
            return ()
            
            


