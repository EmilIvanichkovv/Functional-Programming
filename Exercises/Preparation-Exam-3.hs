
import Data.Maybe ()



data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read,Eq)



data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)
values :: Strategy -> (Tree a) -> [a]
values Inorder t = inorder t
values Preorder t = preorder t
values Postorder t = postorder t

---------------------------------------------
inorder :: Tree a -> [a]
inorder EmptyTree = [] 
inorder t  = (inorder (getLeft t)) ++ [getValue t] ++ (inorder (getRight t))

preorder :: Tree a -> [a]
preorder EmptyTree = [] 
preorder t  = [getValue t] ++ (preorder (getLeft t)) ++ (preorder (getRight t))

postorder  :: Tree a -> [a]
postorder  EmptyTree = [] 
postorder  t  = (postorder (getLeft t)) ++ (postorder (getRight t)) ++ [getValue t]

-------------------------------------------------


safeHead [] = Nothing
safeHead list123 = Just (head list123)

findIndexHelper element arr
  |head arr == element = 0
  |otherwise = 1 + (findIndexHelper element (tail arr))

findIndex :: (Eq a1, Num a2) => a1 -> [a1] -> Maybe a2
findIndex element arr= if (elem element arr) then  (Just (findIndexHelper element arr)) else Nothing


stripPrefix list1 list2
  | (take (length list1) list2) == list1 = Just (drop (length list1) list2)
  | otherwise = Nothing




data NonEmpty a = OneElement a | List {first::a, next::NonEmpty a}  deriving (Show,Read)
test=(List 1 (List 2 (List 3 (OneElement 6))))

getHead (OneElement a) = a
getHead (List a b) = a

getTail (List a b) =  b

getLength (OneElement a) = 1
getLength (List a b) = 1 + (getLength b)

addElement element (OneElement a) = (List a (OneElement element))
addElement element (List a b) = (List a (addElement element b))

reverse' (OneElement a) = (OneElement a)
reverse' (List a b) = (addElement a (reverse' b))


uncons' (List a b) = (a,b)



getValue :: Tree a -> a
getValue (Node v l r)= v
getLeft :: Tree a -> Tree a
getLeft (Node v l r)= l
getRight :: Tree a -> Tree a
getRight(Node v l r)= r


 
testTree1 = (V 5 (V 3 
                        (V 2
                            Empty 
                            Empty) 
                        (V 4
                            Empty 
                            Empty)) 
                   (V 8 
                        (V 6 
                            Empty 
                            (V 7 
                              Empty
                              Empty)) 
                        (V 10 
                            Empty 
                            Empty)))

maxSumPath EmptyTree = 0 
maxSumPath (Node val l r) = max (val + (maxSumPath l)) (val + (maxSumPath r))


prune (EmptyTree) = EmptyTree
prune (Node a EmptyTree EmptyTree) = EmptyTree
prune (Node val l r) = (Node val (prune l) (prune r))


bloom (Node a EmptyTree EmptyTree) = (Node a (Node a EmptyTree EmptyTree) (Node a EmptyTree EmptyTree))
bloom (EmptyTree) = EmptyTree
bloom (Node val l r) = (Node val (bloom l) (bloom r))




mapTree f EmptyTree = EmptyTree
mapTree f (Node x l r)=(Node (f x) (mapTree f l) (mapTree f r))



data BST a = Empty | V {
                            x :: a,
                            l :: BST a,
                            r :: BST a
                          } deriving (Show,Read,Eq)

bstInsert element Empty = (V element Empty Empty)
bstInsert element (V a l r) = (if (a > element) then (V a (bstInsert element l) r) else (V a l (bstInsert element r)))


bstSearch element Empty = False
bstSearch element (V a l r) = (if element == a then True else (if (a > element) then (bstSearch element l) else (bstSearch element r)))


bstSearch' element Empty = False
bstSearch' element (V a l r)
  |element == a = True
  |a > element  = bstSearch' element l
  |otherwise = bstSearch' element r


bstValues Empty = []
bstValues (V x l r) = (bstValues l) ++ [x] ++(bstValues r)


bstSize Empty = 0
bstSize (V x l r) = (bstSize l) + 1 + (bstSize r)


bstSort' arr bstTree
  |null arr = bstValues bstTree
  |otherwise = (bstSort' (tail arr) (bstInsert (head arr) bstTree))


bstSort arr = bstSort' arr Empty



data Direction = LeftD | RightD deriving (Show,Read,Eq)


bstPath' element Empty = []
bstPath' element (V a l r)
  |element == a = []
  |a > element  = [LeftD] ++ (bstPath' element l)
  |otherwise    = [RightD] ++ (bstPath' element r)


bstPath element bstTree = (if bstSearch element bstTree then Just(bstPath' element bstTree) else Nothing)



data TreeMap a b = EmptyMap | Map { pair::(a,b) , leftMap:: TreeMap a b, rightMap:: TreeMap a b } deriving (Show,Read,Eq)



mapInsert (key,value) EmptyMap = (Map (key,value) EmptyMap EmptyMap)
mapInsert (key,value) (Map pair l r)
  |key == (fst pair) =  (Map (key,value) l r)
  |key > (fst pair) = (Map pair l (mapInsert (key,value) r))
  |otherwise = (Map pair (mapInsert (key,value) l) r)
  

mapSearch' key EmptyMap = False
mapSearch' key (Map pair l r)
  |key == (fst pair) = True
  |key > (fst pair) = mapSearch' key r
  |otherwise = mapSearch' key l

mapSearch'' key (Map pair l r)
  |key == (fst pair) = (snd pair)
  |key > (fst pair) = mapSearch'' key r
  |otherwise = mapSearch'' key l

mapSearch key (Map pair l r)= (if (mapSearch' key (Map pair l r)) then (Just (mapSearch'' key (Map pair l r))) else Nothing)


data Expr a = X |Number {val::a} | Addition{val1::Expr a ,val2::Expr a}  | Subsraction{val1::Expr a ,val2::Expr a} | Multiply{val1::Expr a ,val2::Expr a} | Division {val1::Expr a ,val2::Expr a}



--eval (Addition (Multiply (Number 5) X) (Number 3)) x
eval (Number a) x = a
eval (X) x = x
eval (Addition a b) x = (eval a x) + (eval b x)
eval (Subsraction a b) x = (eval a x) - (eval b x)
eval (Multiply a b) x = (eval a x) * (eval b x)
eval (Division a b) x = (eval a x) / (eval b x)

quicksort:: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
  where 
    lesser = filter (<x) xs
    greater = filter (>= x) xs

    