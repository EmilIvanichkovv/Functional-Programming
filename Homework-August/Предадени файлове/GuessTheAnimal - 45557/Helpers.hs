module Helpers where

import Data.Text.Encoding
import Tree

-- Get value of current node. Basically the questions.
getQuestion :: Tree String -> String
getQuestion sTree = do
    question <- value sTree
    return question

-- Asks for answer of given question.
getAnswer :: String -> IO String
getAnswer question  = do 
    putStrLn "да / не"
    answer <- getLine
--    putStrLn "\n"
    return  answer

-- Asks for new animal which is not in the data base.
getNewAnimal :: IO String
getNewAnimal = do 
    putStrLn "Предавам се. Кое е животното?" 
    answer <- getLine
    return answer
    
-- Asks for question about the new animal which is not in the data base.
getQuestionForNewAnimal :: String -> String -> IO String
getQuestionForNewAnimal oldAnimal newAnimal = do
    putStrLn $ "С какъв въпрос мога да различа " ++  oldAnimal ++ " от " ++  newAnimal ++ "?"
    newQuestion <- getLine
    return newQuestion

skipQuestionMark :: String -> String
skipQuestionMark "" = ""
skipQuestionMark (x:xs)
    |x /= '?'=  x : skipQuestionMark xs
    | otherwise = ""
