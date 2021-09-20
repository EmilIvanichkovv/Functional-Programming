module ReadWrite where

import System.IO

-- Dealing with cyrillic symbols

-- Re-writing readFile function in order to deal with a problem with reading and writing at same file.
-- source: https://github.com/ndmitchell/shake/issues/37
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h  >>= \s -> length s `seq` return s

readFileStrict :: String -> IO String
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
    