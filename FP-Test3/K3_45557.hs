{--
  СУ "Св. Климент Охридски"
  Факултет по математика и информатика
  Курс Функционално програмиране 2020/21
  Контролно 3
  2021-01-16

  Име: Емил Иваничков
  ФН: 45557
  Специалност: Информатика
  Курс: 3
  Административна група: 2 
  Начален час на контролното: 7:45
--}


module K3_45557 where

-- 1) 
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)
                           
sampleTree = Node 'a' (Node 'b' (Node 'd' EmptyTree
                                          (Node 'g' EmptyTree EmptyTree))
                                (Node 'e' EmptyTree EmptyTree))
                      (Node 'c' EmptyTree
                                (Node 'f' EmptyTree EmptyTree))                          


{-
treeWords :: Tree Char -> [String]
treeWords EmptyTree = []
treeWords (Node x l r)
-}


makeWord EmptyTree lst = ""
makeWord (Node x EmptyTree EmptyTree) lst = lst
makeWord (Node x l r) lst = makeWord l (lst ++ [x]) ++ makeWord r (lst ++ [x])


krai (Node x EmptyTree EmptyTree) = True
krai (Node x _ _) = False