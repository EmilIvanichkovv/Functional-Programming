module GamePlay where

import Tree
import ReadWrite
import Helpers

-- For each turn ask question according to current subTree, take answer and call next turn events.
turn :: String -> Tree String -> Tree String -> IO ()
turn fileName base sTree  = do
    let question =  getQuestion sTree
    putStrLn $ (skipQuestionMark question) ++ "?"
    answer <- getAnswer question
    nextTurn fileName base sTree answer

-- Covers different cases for the answer
-- - If the question is a guess and it is right - ends game
-- - If the question is a guess and it is wrong - adds new animal
-- - If the question is characteristics of animal - calls 'turn' wiht left or right subTree according to the answer
nextTurn :: String -> Tree String -> Tree String -> String -> IO ()
nextTurn fileName _ (Node _ EmptyTree EmptyTree) "да" =  putStrLn $ "Познах! Край на играта" ++ "\n"
nextTurn fileName base (Node x EmptyTree EmptyTree) "не" = do 
    newAnimal <- getNewAnimal
    newQuestion <- getQuestionForNewAnimal x newAnimal
    let newTree = treeEnrichment x base (treeWithNewInfo newQuestion newAnimal (Node x EmptyTree EmptyTree) )
    myWrite fileName (treeToString newTree)
    putStrLn $ "Записвам!" ++ "\n"
nextTurn fileName base (Node _ l r) answer 
    |answer == "да" = turn fileName base l 
    |answer == "не" = turn fileName base r 
--    |(answer == "Da") && (l == EmptyTree) = putStrLn "ok"
    |otherwise = putStrLn "Некоректен отговор!"


-- Gives possibility to start new game after previous ended or input was incorrect
newGame  :: String ->IO ()
newGame fileName = do
    putStrLn "Искаш ли да започнеш нова игра?"
    putStrLn "да / не"
    answer <- getLine
    startAgainOrQuit answer
        where
            startAgainOrQuit "да" = play fileName
            startAgainOrQuit "не" = putStrLn "До скоро!"
            startAgainOrQuit _ = do 
                putStrLn "Некоректен отговор!"
                newGame fileName

-- Starts game
play :: String -> IO ()
play fileName = do
    putStrLn "\n"
    putStrLn "ИГРАТА ЗАПОЧВА"
    putStrLn $  "Намисли си животно!" ++ "\n"
    s <- (treeFromFile fileName)
    turn fileName s s
    newGame fileName