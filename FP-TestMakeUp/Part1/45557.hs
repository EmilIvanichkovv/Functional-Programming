{--
  СУ "Св. Климент Охридски"
  Факултет по математика и информатика
  Курс Функционално програмиране 2020/21
  Поправителна сесия 2020/21

  Име: Емил Иваничков
  ФН: 45557
  Специалност: Информатика
  Курс: 3
  Административна група: 2
  Дата: 16.08.2021
  Начален час на контролното за вашата група: 8:00 
--}

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

treeWords :: Tree String -> [[String]]
treeWords EmptyTree = []
treeWords (Node v EmptyTree EmptyTree) = [[v]]
treeWords (Node v l r)  = map (v:) (wl ++ wr)
    where wl = treeWords l
          wr = treeWords r                   

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) =  lesser ++ [x] ++ greater
    where
        lesser   = filter (<x) xs
        greater  = filter (>=x) xs