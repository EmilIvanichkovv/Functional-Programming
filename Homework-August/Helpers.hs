module Helpers where

import Data.Text.Encoding
import Tree
    
getQuestion :: Tree String -> String
getQuestion sTree = do
    question <- value sTree
    return question

getAnswer :: String -> IO String
getAnswer question  = do 
    putStrLn "Da / Ne"
    answer <- getLine
--    putStrLn "\n"
    return  answer

getNewAnimal :: IO String
getNewAnimal = do 
    putStrLn "Predavam se. Koe e jivotnoto" 
    answer <- getLine
    return answer

getQuestionForNewAnimal :: String -> String -> IO String
getQuestionForNewAnimal oldAnimal newAnimal = do
    putStrLn $ "Kak moga da razlicha " ++  oldAnimal ++ " ot " ++  newAnimal 
    newQuestion <- getLine
    return newQuestion

skipQuestionMark :: String -> String
skipQuestionMark "" = ""
skipQuestionMark (x:xs)
    |x /= '?'=  x : skipQuestionMark xs
    | otherwise = ""
