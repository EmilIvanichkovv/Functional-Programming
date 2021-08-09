module ReadWrite where

import System.IO


hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h  >>= \s -> length s `seq` return s

readFileStrict :: FilePath -> IO String
readFileStrict name = do
    inputHandle <- openFile name ReadMode 
    hSetEncoding inputHandle utf8
    hGetContentsStrict inputHandle
{-# INLINE readFileStrict #-}


myWrite :: String -> String -> IO ()
myWrite file outPut = do
    outputHandle <- openFile file WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle outPut
    hClose outputHandle
    