module Validation where

import ReadWrite
import GamePlay
import Helpers
import System.Directory

validation :: String -> IO ()
validation fileName = emptyFile fileName

emptyFile :: String -> IO ()
emptyFile fileName = do
   content <- readFileStrict fileName
   check content
    where
        check x 
            | x == "" = do
                putStrLn "Некоректен файл! Файлът е празен!"
                putStrLn "Програмата ще започне с файла по подразбиране"
                copyFile "defaultDataBase.txt" "v_emptyFileFix.txt"  
                play "v_emptyFileFix.txt"
            |otherwise = firstBracket fileName

firstBracket :: String ->  IO ()
firstBracket fileName = do 
    content <- readFileStrict fileName
    check content 
     where
        check (x:xs)
            | x /= '(' = do
                 putStrLn "Некоректен файл! Файлът не започва коректно!"
                 putStrLn "Програмата ще започне с файла по подразбиране"
                 copyFile "defaultDataBase.txt" "v_firstBracketFix.txt" 
                 play "v_firstBracketFix.txt"
            | otherwise = bracketsCount fileName

bracketsCount :: String -> IO ()
bracketsCount fileName = do
    content <- readFileStrict fileName
    bracketCheck 0 content
    where
        bracketCheck count ('(':xs) = bracketCheck (count + 1) xs
        bracketCheck count (')':xs) = bracketCheck (count - 1) xs
        bracketCheck count (_:xs) = bracketCheck count xs
        bracketCheck 0 "" = do
            putStrLn "Файлът е коректен!"
            play fileName
        bracketCheck count "" = do 
            putStrLn "Файлът не е коректен!"
            putStrLn "Програмата ще започне с файла по подразбиране"
            copyFile "defaultDataBase.txt" "v_bracketsCountFix.txt" 
            play "v_bracketsCountFix.txt"
 
 
start :: String -> IO ()
start fileName = validation fileName