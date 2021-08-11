module Tree where

import System.IO
import Data.Char
import ReadWrite

-- Tree Struct
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read,Eq)

-- Creates string with the content of the tree ( Root -> Left -> Right ) 
treeToString :: Tree String -> String
treeToString EmptyTree = "*"
treeToString (Node x l r) = "(" ++ x ++ treeToString l ++ treeToString r ++ ")" 

-- Creates Tree from String:
--  Takes substring which corresponds to the left subtree
makeLeftSubtree:: Int -> Bool -> String -> String
makeLeftSubtree counter flag (x:xs)
    | flag == True && counter == 0 = ""
    | x == '('                     = x : ( makeLeftSubtree  (counter + 1) True xs )
    | x == '*' && counter == 0     = "*"
    | flag == False                = makeLeftSubtree counter flag xs
    | x == ')'                     = x : ( makeLeftSubtree (counter - 1) flag xs )
    | otherwise                    = x : ( makeLeftSubtree counter flag xs )

--  Takes substring which corresponds to the right subtree
makeRightSubtree:: Int -> Bool -> String -> String
makeRightSubtree counter flag (x:xs)
    | flag == True && counter == 0 = makeLeftSubtree 0 False (x:xs)
    | x == '('                     = makeRightSubtree  (counter + 1) True xs 
    | x == '*' && counter == 0     = "*"
    | flag == False                = makeRightSubtree counter flag xs
    | x == ')'                     = makeRightSubtree (counter - 1) flag xs
    | otherwise                    = makeRightSubtree counter flag xs 

--  Creates Tree from given String:
parseTree :: String -> Tree String
parseTree (x:xs) 
    | x == '*' = EmptyTree
    | x == '(' = (Node (takeValue xs) (parseTree (makeLeftSubtree 0 False xs)) (parseTree (makeRightSubtree 0 False xs)))
    | otherwise = parseTree xs

-- Takes value for each Node of the Tree
takeValue :: String -> String
takeValue "" = ""
takeValue (x:xs)
    | x /= '(' && x/=')' && x/='*' =  x : takeValue xs
    | otherwise = ""


-- Creates Tree form content of given correct file
treeFromFile :: String -> IO (Tree String)
treeFromFile fileName  = do
    contents <- readFileStrict fileName
    let tree_result =  parseTree contents
    return tree_result

-- Other way. Trying to understand monads

--  ioTree :: String -> IO (Tree String)
--  ioTree contents = return (parseTree contents)

--  readFromFile1 :: String -> IO (Tree String)
--  readFromFile1 fileName  = do
--      readFile fileName
--      >>= ioTree
--      >>= \contents-> return (parseTree contents)    


-- Here we enrich the data base
--  "Bloom" exact leaf - Add new question as root, new and old animal as leaves"
treeWithNewInfo :: String -> String -> Tree String -> Tree String   
treeWithNewInfo  question newAnimal (Node x EmptyTree EmptyTree) = Node question (Node newAnimal EmptyTree EmptyTree) (Node x EmptyTree EmptyTree)

-- "Еnriches" current Tree with the new info ( given as Tree )
treeEnrichment :: String -> Tree String -> Tree String -> Tree String 
treeEnrichment curAnimal EmptyTree newTree = EmptyTree
treeEnrichment curAnimal (Node y l r) newTree
    | y /= curAnimal = (Node y (treeEnrichment curAnimal l newTree) (treeEnrichment curAnimal r newTree))
    | y == curAnimal = newTree

