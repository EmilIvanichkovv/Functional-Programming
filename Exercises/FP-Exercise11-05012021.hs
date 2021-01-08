-- Usefull functions:
-- map, filter, reverse, length, null, elem, take, drop, zip, zipWith, takeWhile, dropWhile

-- Задача 0
myMap f lst 
    | null lst = []
    | otherwise = f (head lst) : myMap f (tail lst)

myFilter p lst
    | null lst = []
    | otherwise = if p (head lst) then (head lst): myFilter p (tail lst) else myFilter p (tail lst)

myReverse lst 
    | null lst = []
    | otherwise = (myReverse (tail lst)) ++ [(head lst)]

myLength lst
    | null lst = 0
    | otherwise  = 1 + myLength (tail lst)

myNull lst
    | lst == [] = True 
    | otherwise = False

myElem el lst
    | null lst = False 
    | otherwise = el == (head lst) || (myElem el (tail lst))

myTake x lst 
    | null lst = []
    | x == 0 = [] 
    | otherwise = (head lst) : myTake (x-1) (tail lst)

myDrop x lst 
    | null lst = []
    | x == 0 = lst
    | otherwise = myDrop (x-1) (tail lst)

myZip lst1 lst2
    | null lst1 || null lst2 = []
    | otherwise = ((head lst1),(head lst2)):myZip (tail lst1) (tail lst2)

myZip2 lst1 lst2
    | null lst1 || null lst2 = []
    | otherwise = ((head lst1),(head lst2)):myZip (tail lst1) (tail lst2)

myZipWith f lst1 lst2
    | null lst1 || null lst2 = []
    | otherwise = (f (head lst1) (head lst2)):myZipWith f (tail lst1) (tail lst2)

myTakeWhile p lst 
    | null lst = []
    | otherwise = if p (head lst) then  (head lst) : myTakeWhile p (tail lst) else []

myDropWhile p lst
    | null lst = []
    | otherwise = if p (head lst) then myDropWhile p (tail lst) else lst


-- Задача 1
minimum' lst = foldr (\ el res -> (min el res)) (head lst)lst
minimum1' (x:xs) = foldl min x xs 

maximum' (x:xs) = foldr max x xs

reverse' lst = foldr (\ el res ->  res ++ [el]) [] lst -- ламбдата е еквивалентна на (flip (:))

length' lst = foldr (\ el res -> 1+res) 0 lst

all' p lst = foldr (\ el res -> p el && res) True lst
any' p lst = foldr (\ el res -> p el || res) False lst

append' lst1 lst2 = foldr (:) lst2 lst1

replicate' x expr = foldr (\ el res -> expr : res) []  [1 .. x]

--   List comprehension:

-- [ x | x се генерира от някакво множество, някакви условия]
-- [ x | x <- [1..10], x > 5]
-- [ x*x | x<-[1..10], x > 5]
-- length [ x | x<-[1..10], x > 5]
-- [ x*y | x <- lst1, y <- lst2]

f lst = [ x | x <-lst, even x, x > 5]
-- би било еквиваленто да го направим с филтър

-- Задача 2
countDivisors n = length [ d | d <- [2..n-1], mod n d == 0]
prime n 
    | n==1 = False 
    |otherwise = (countDivisors n) == 0
descartes lst1 lst2 = [ (x,y) | x<-lst1, y<-lst2 ]

-- Задача 3
primes = [ x | x <- [1 ..], prime x]
primes1 = filter prime [2..]

--Задача 4
primesEratosten = f [2 ..]
    where f (x:xs) =  x : f [ y | y <- xs, mod y x /=0]
--or
helpFilter number = (\ a ->  (mod a number)/=0)
eratosten arr = [(head arr)] ++(eratosten (filter (helpFilter (head arr)) (tail arr)))

-- Задача 5 
{-
Всеки следващ генератор отговаря на вложен цикъл в императивните езици
Ако някой вътрешен цикъл е безкраен, то ще забием завинаги в него
и няма да преминем на следващите итерации на външиния
[ (x,y) | x<-[0..], y<-[0..] ]
  съответства грубо на:
for (x = 0; ; ++x) {
    for (y = 0; ; ++y) {
        ...
    }
}
Решение: ще генерираме наредените двойки по "диагонали":
(0,0) (1,0) (2,0) (3,0) (4,0) ...
(0,1) (1,1) (2,1) (3,1) ...
(0,2) (1,2) (2,2) (3,2) ...
(0,3) (1,3) (2,3) ...
...
-> [(0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3), ...
-}

infDoubles = [ ((x-y),y) | x <- [0..], y <- [0..x] ]
--or
helpPairGenerate (x,y)
    | y == 0    = [(x,y)]
    | otherwise = [(x,y)] ++ helpPairGenerate ((x+1),(y-1))
pairs (x,y) = helpPairGenerate (x,y) ++ (pairs (x,(y+1)))
infDoubles' = pairs (0,0)

-- Засача 6
infPythagoreans = [ (y,x,z) | x <- [1..], y <- [1..x], z <- [1..(x+y)], x^2 + y^2 == z^2 ]

-- Задача 7
compress [] = []
compress lst = ( head lst, length firstPart) : compress rest
    where (firstPart, rest) = span(\x -> x == head lst) lst

-- Задача 8
maxRepeated lst = foldr (\ (_,el) res -> max el res) 0 (compress lst)
-- or
maxRepeated' lst = maximum [ el | (_,el) <- compress lst]
--or
maxRepeated'' lst = maximum (map snd (compress lst))

-- Задача 9
uniques lst 
    | lst == [] = []
    | otherwise = if elem (head lst) (tail lst) then uniques (tail lst) else (head lst):uniques (tail lst) 
--or 
makeSet lst = foldr (\ el res -> if el `elem` res then res else el:res) [] lst

 -- Задача 10
histogram lst = [ (el, count el) | el <- uniques lst]
    where count el = length [ count | count <- lst, count == el]
--or 
numberOfElements elem l1
    | null l1 = 0
    | otherwise =  if (head l1) == elem then 1 + numberOfElements elem (tail l1) else numberOfElements elem (tail l1)

removeElement elem l
    | null l = []
    | head l == elem = removeElement elem (tail l)
    | otherwise = (head l):(removeElement elem (tail l))

histogram' l1
    | null l1 = []
    | otherwise = ((head l1) , (numberOfElements (head l1) l1)) : (histogram' (removeElement (head l1) l1))


-- Задача 11
distance (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
maxDistance pts = maximum [ distance p1 p2 | p1<-pts, p2<-pts ]
