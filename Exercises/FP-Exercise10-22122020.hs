x = 5
y = 10

-- Examples: 

-- True
-- False
-- True || False
-- True && False
-- not False
-- 3 == 3
-- 3 /= 3
-- 3 * 3
-- 3 / 3
-- 3 ** 3
-- 3 ^ 3
-- div 5 2
-- mod 5 2
-- sqrt 4
-- 5 'mod' 2
--  not (3 == 3)
-- elem 3 [1,2,3,4]
-- 3 'elem' [1, 2, 3, 4]
--'this is char'

-- let p = (2, 3)
-- let p = (2, 'a)'
-- fst p -> 2
-- snd p -> 'a'


firstFunc x y z = x + y * z

-- Задача 1
fib n = if n < 2 then n else fib (n-1) + fib (n-2)

-- Задача 2
fib2 n 
    | n < 2 = n
    | n == 5 = 5
    |otherwise = fib2 (n-1) + fib2 (n-2) 

fib3 0 = 0
fib3 1 = 1
fib3 n = fib3 (n-1) + fib3 (n-2)

fib4 0 = 0
fib4 1 = 1
fib4 n = fib4 (n-1) + fib4 (n-2)

fib5 n = case n of 0 -> 0
                   1 -> 1
                   _ ->  fib5 (n-1) + fib5 (n-2)


isA 'a' = True
isA 'A' = True 
isA _ = False 

-- foo x = if (fib x) > 100 then (fib x) else 42 //not ok to call (fib x) twice
foo x = let y = fib x in
            if y > 100 then y else 42
-- or better
bar x = if y > 100 then y else (42 +z)
    where y = fib x
          z = 2*(fact y) + x
          fact 0 = 1
          fact n = n* fact(n-1)



-- Задача 3
myPow1 x y = if y == 0 then 1 else x * myPow1 x (y-1)

myPow2 x y
    | y == 1 = x
    |otherwise = x * myPow2 x (y-1)

myPow3 x 1 = x
myPow3 x y = x * myPow3 x (y-1)

fastPow _ 0 = 1
fastPow x 1 = x
fastPow x n
  | even n    = half*half
  | otherwise = half*half*x
  where half = fastPow x (n `div` 2)

-- Задача 4
complAdd p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)
complAdd2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)

complSub p1 p2 = (fst p1 - fst p2, snd p1 - snd p2)
complSub2 (x1,y1) (x2,y2) = (x1-x2, y1-y2)

complMult p1 p2 = (fst p1 * fst p2 - snd p1 * snd p2, fst p1 * snd p2 + snd p1 * fst p2)
complMult2 (x1,y1) (x2, y2) = (x1*x2-y1*y2, x1*y2+y1*x2)


-- Задача 5
distance (x1, y1) (x2, y2) = sqrt ((myPow1 (x1-x2) 2) + (myPow1 (y1-y2) 2))

-- Lambda in Haskell:
-- (\ x -> x*x) 6  ->  36

-- Задача 6
repeated _ 0 = \x -> x
repeated f n = \x -> f ((repeated f (n-1)) x)

repeated2 f n 
    | n == 0    = \x -> x
    | otherwise = \x -> f ((repeated f (n-1)) x)


-- Lists:

--[]
--[1,2,3,4,5]
-- head
-- tail
-- null
-- 1 : [2,3,4] -> [1,2,3,4]

-- Range:
-- [1..5] -> [1,2,3,4,5]
-- [1, 3 .. 11] -> [1,3,5,7,9,11]
-- [2,4 .. 7] -> [2,4,6]
-- [10,9 .. 1] -> [10,9,8,7,6,5,4,3,2,1]
-- [1 ..] --> promise for infinite list
-- Index in list with !! 

length' lst = if null lst then 0 else 1 + length'(tail lst)

length2 [] = 0
length2 (x:xs) = 1 + length2 xs

length3 [] = 0
length3 (_:xs) = 1 + length' xs
