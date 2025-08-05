-- Q1
-- second :: [a] -> a
-- swap2 :: (a,b) -> (b,a)
-- pair :: a -> b -> (a,b)
-- palindrome :: Eq a => [a] -> Bool
twice :: (a -> a) -> a -> a 
twice f x = f (f x) -- Ex: f*f*x

-- Q2
prodMn :: Int -> Int -> Int
prodMn m n
   | m>n = error "m eh maior que n"
   | m==n = m
   | otherwise = m * prodMn (m+1) n
   
sumQuad :: Int -> Int
sumQuad n 
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = (n^2) + sumQuad (n-1)

sumFats :: Int -> Int
sumFats n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = product [1..n] + sumFats (n-1)

duplicate :: Int -> String -> String
duplicate 0 _ = []
duplicate n xs = xs ++ duplicate (n-1) xs

makespaces :: Int -> String
makespaces n = duplicate n " "

-- NAO CONSEGUI A LETRA F
-- rjustify :: Int -> String -> String
-- rjustify n [x] = (makespaces n " ") ++ [x]

-- Q3
fat :: Integer -> Integer
fat a = fat' a
   where 
      fat' 0 = 0
      fat' 1 = 1
      fat' a = a * fat' (a-1)

reverse2 :: [a] -> [a]
reverse2 xs = reverse' xs
   where 
      reverse' [] = []
      reverse' (x:xs) = reverse' xs ++ [x]

-- Q5
until2 :: (a -> Bool) -> (a -> a) -> a -> a
until2 p f x = if p x then x else until2 p f (f x)

-- Q6
retornaSup :: Int -> [Int] -> Int
retornaSup n [] = 0
retornaSup n (x:xs)
   | n < x = 1 + retornaSup n xs
   | otherwise = retornaSup n xs
   
-- Q7
retornaListaSup :: Int -> [Int] -> [Int]
retornaListaSup n [] = []
retornaListaSup n (x:xs)
   | n < x = x : retornaListaSup n xs
   | otherwise = retornaListaSup n xs
   
-- Q9


-- Q42
num_tri :: Int -> [Int]
num_tri 0 = [0]
num_tri n = num_tri (n-1) ++ [n*(n+1) `div` 2];

-- Q44 easy KKKKKKKKKKKK
dec2bin :: Int -> [Int]
dec2bin n
   | n == 0 = [0]
   | n == 1 = [1]
   | otherwise = dec2bin (n `div` 2) ++ [n `mod` 2]
