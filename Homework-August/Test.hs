
module Test where

import System.IO
import Data.Char
import Data.Maybe (isJust, isNothing)

 data Tree a = EmptyTree | Node {
                             value :: a,
                             left  :: Tree a,
                             right :: Tree a
                           } deriving (Show,Read,Eq)
 makeLeftSubtree:: Int -> Bool -> String -> String
 makeLeftSubtree counter flag (x:xs)
     | flag == True && counter == 0 = ""
     | x == '('                     = x : ( makeLeftSubtree  (counter + 1) True xs )
     | x == '*' && counter == 0    = "*"
     | flag == False                = makeLeftSubtree counter flag xs
     | x == ')'                     = x : ( makeLeftSubtree (counter - 1) flag xs )
     | otherwise                    = x : ( makeLeftSubtree counter flag xs )
 
 makeRightSubtree:: Int -> Bool -> String -> String
 makeRightSubtree counter flag (x:xs)
     | flag == True && counter == 0 = makeLeftSubtree 0 False (x:xs)
     | x == '('                     = makeRightSubtree  (counter + 1) True xs 
     | x == '*' && counter == 0     = "*"
     | flag == False                = makeRightSubtree counter flag xs
     | x == ')'                     = makeRightSubtree (counter - 1) flag xs
     | otherwise                    = makeRightSubtree counter flag xs 
 
 parseTree :: String -> Tree String
 parseTree (x:xs) 
  -- let counter = 0
     | x == '*' = EmptyTree
     | x == '(' = (Node (takeSmt xs) (parseTree (makeLeftSubtree 0 False xs)) (parseTree (makeRightSubtree 0 False xs)))
     |otherwise = parseTree xs
 
 takeSmt :: String -> String
 takeSmt "" = ""
 takeSmt (x:xs)
     |x /= '(' && x/=')' && x/='*' =  x : takeSmt xs
     | otherwise = ""

hi = do 
    putStrLn "Hello World"