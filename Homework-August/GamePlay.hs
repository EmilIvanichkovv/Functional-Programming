module GamePlay where

import Tree
import ReadWrite
import Helpers

nextTurn ::Tree String -> Tree String -> String -> IO ()
nextTurn _ (Node _ EmptyTree EmptyTree) "да" =  putStrLn "Познах! Край на играта"
nextTurn base (Node x EmptyTree EmptyTree) "не" = do 
    newAnimal <- getNewAnimal
    newQuestion <- getQuestionForNewAnimal x newAnimal
    let newTree = treeEnrichment x base (treeWithNewInfo newQuestion newAnimal (Node x EmptyTree EmptyTree) )
    myWrite "dataBase.txt" (treeToString newTree)
    putStrLn "Записвам!"
nextTurn base (Node _ l r) answer 
    |answer == "да" = turn base l 
    |answer == "не" = turn base r 
--    |(answer == "Da") && (l == EmptyTree) = putStrLn "ok"
    |otherwise = putStrLn "Некоректен отговор!"


turn ::Tree String -> Tree String -> IO ()
turn base sTree  = do
    let question =  getQuestion sTree
    putStrLn $ (skipQuestionMark question) ++ "?"
    answer <- getAnswer question
    nextTurn base sTree answer


newGame  :: IO ()
newGame = do
    putStrLn "Искаш ли да започнеш нова игра?"
    putStrLn "да / не"
    answer <- getLine
    startAgainOrQuit answer
        where
            startAgainOrQuit "да" = play "dataBase.txt"
            startAgainOrQuit "не" = putStrLn "До скоро!"
            startAgainOrQuit _ = do 
                putStrLn "Некоректен отговор!"
                newGame
play :: String -> IO ()
play fileName = do
    putStrLn "\n"
    putStrLn "ИГРАТА ЗАПОЧВА"
    putStrLn $  "Намисли си животно!" ++ "\n"
    s <- (treeFromFile fileName)
    turn s s
    newGame