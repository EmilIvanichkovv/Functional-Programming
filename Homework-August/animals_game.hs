import System.IO
import Data.Char
import System.Directory
import Test
import Data.Maybe (isJust, isNothing)

hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s

readFileStrict :: FilePath -> IO String
readFileStrict name =  openFile name ReadMode >>= hGetContentsStrict
{-# INLINE readFileStrict #-}


data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read,Eq)

treeToString :: Tree String -> String
treeToString EmptyTree = "*"
treeToString (Node x l r) = "(" ++ x ++ treeToString l ++ treeToString r ++ ")" 

-- Tree to String
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
    | x == '(' = (Node (takeValue xs) (parseTree (makeLeftSubtree 0 False xs)) (parseTree (makeRightSubtree 0 False xs)))
    |otherwise = parseTree xs

takeValue :: String -> String
takeValue "" = ""
takeValue (x:xs)
    |x /= '(' && x/=')' && x/='*' =  x : takeValue xs
    | otherwise = ""



treeFromFile :: String -> IO (Tree String)
treeFromFile fileName  = do
    contents <- readFileStrict fileName
    let tree_result =  parseTree contents
    return tree_result

-- ioTree :: String -> IO (Tree String)
-- ioTree contents = return (parseTree contents)
-- 
-- readFromFile1 :: String -> IO (Tree String)
-- readFromFile1 fileName  = do
--     readFile fileName
--     >>= ioTree
--     >>= \contents-> return (parseTree contents) 
--     
myCopyFile :: String -> String -> IO()
myCopyFile source update = do 
    content <- (readFile source)
    writeFile update content
 
-- Creates new Tree -
--  "Bloom" exact leaf - Add new question as root, new and old animal as leaves"
treeWithNewInfo :: String -> String -> Tree String -> Tree String   
treeWithNewInfo  question newAnimal (Node x EmptyTree EmptyTree) = Node question (Node newAnimal EmptyTree EmptyTree) (Node x EmptyTree EmptyTree)

-- "Еnriches" current Tree with the new info ( given as Tree )
treeEnrichment :: String -> Tree String -> Tree String -> Tree String 
treeEnrichment curAnimal EmptyTree newTree = EmptyTree
treeEnrichment curAnimal (Node y l r) newTree
    | y /= curAnimal = (Node y (treeEnrichment curAnimal l newTree) (treeEnrichment curAnimal r newTree))
    | y == curAnimal = newTree

start =  (Node "Bozainik" 
    (Node "Golqmo" 
        (Node "Slon" EmptyTree EmptyTree ) 
        (Node "Kotka" EmptyTree EmptyTree ) )
    (Node "Leti" 
        (Node "Pchela" EmptyTree EmptyTree ) 
        (Node "Zlatna ribka" EmptyTree EmptyTree ) ))
start1 = (Node "Pchela" EmptyTree EmptyTree )
as = EmptyTree

getQuestion :: Tree String -> String
getQuestion sTree = do
    question <- value sTree
    return question

getAnswer :: String -> IO String
getAnswer question  = do 
    putStrLn "Da / Ne"
    answer <- getLine
    return  answer

getNewAnimal :: IO String
getNewAnimal = do 
    putStrLn "Predavam se. Koe e jivotnoto" 
    answer <- getLine
    return answer

getQuestionForNewAnimal :: String -> String -> IO String
getQuestionForNewAnimal oldAnimal newAnimal = do
    putStrLn $ "Koq harakteristika razlichava " ++  oldAnimal ++ " ot " ++  newAnimal 
    newQuestion <- getLine
    return newQuestion


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
    putStrLn $ question ++ " li e?"
    answer <- getAnswer question
    nextTurn base sTree answer

gamePlay :: String -> IO ()
gamePlay fileName = do
    s <- (treeFromFile fileName)
    turn s s
  -- myCopyFile "dummyFile.txt" "dataBase.txt"
updateSourceFile = do
    removeFile "dataBase.txt"
    renameFile "test.txt" "dataBase.txt"
-- main :: IO()
main = do 
    putStrLn "Namisli si jivotno!"
    hi
    gamePlay "dataBase.txt"
   -- updateSourceFile
