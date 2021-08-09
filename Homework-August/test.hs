


import System.IO

myRead = do
   
    inputHandle <- openFile "dataBase.txt" ReadMode 
    hSetEncoding inputHandle utf8
    theInput <- hGetContents inputHandle
    outputHandle <- openFile ("a"++"dataBase.txt") WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle theInput
    hClose outputHandle