module GamePlay where

import Tree
import Helpers

nextTurn ::Tree String -> Tree String -> String -> IO ()
nextTurn _ (Node _ EmptyTree EmptyTree) "Da" =  putStrLn "Pozah! Krai na igrata!"
nextTurn base (Node x EmptyTree EmptyTree) "Ne" = do 
    newAnimal <- getNewAnimal
    newQuestion <- getQuestionForNewAnimal x newAnimal
    let newTree = treeEnrichment x base (treeWithNewInfo newQuestion newAnimal (Node x EmptyTree EmptyTree) )
    writeFile "dataBase.txt" (treeToString newTree)
    putStrLn "Zapisvam!"
nextTurn base (Node _ l r) answer 
    |answer == "Da" = turn base l 
    |answer == "Ne" = turn base r 
--    |(answer == "Da") && (l == EmptyTree) = putStrLn "ok"
    |otherwise = putStrLn "Greshen Vhod"


turn ::Tree String -> Tree String -> IO ()
turn base sTree  = do
    let question =  getQuestion sTree
    putStrLn $ (skipQuestionMark question) ++ "?"
    answer <- getAnswer question
    nextTurn base sTree answer


newGame  :: IO ()
newGame = do
    putStrLn "Iskash li da igrash nova igra?"
    putStrLn "Da / Ne"
    answer <- getLine
    startAgainOrQuit answer
        where
            startAgainOrQuit "Da" = play "dataBase.txt"
            startAgainOrQuit "Ne" = putStrLn "Dovijdane"
            startAgainOrQuit _ = do 
                putStrLn "Greshen Vhod"
                newGame
play :: String -> IO ()
play fileName = do
    putStrLn "\n"
    putStrLn "IGRATA ZAPOCHVA"
    putStrLn $  "Namisli si jivotno!" ++ "\n"
    s <- (treeFromFile fileName)
    turn s s
    newGame