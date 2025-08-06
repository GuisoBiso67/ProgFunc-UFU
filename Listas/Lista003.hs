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
   
-- Q10
-- estrutura: [expressão_de_saida | gerador, filtro]
intersecao :: Eq t => [t] -> [t] -> [t]
intersecao as bs = [a | a <- as, (elem a bs)]

-- Q11
returnNeg :: (Num t, Ord t) => [t] -> [t]
returnNeg as = [a | a <- as, a<0]

-- Q13
tabuada :: Int -> [(Int, Int, Int)]
tabuada n = [(n,i,n*i) | i <- [1..10]]

-- Q14
transf :: [a] -> [a]
transf xs
   | length xs<4 = xs
   | otherwise =
      let n           = length xs
          first       = xs !! 0
          second      = xs !! 1
          penultimate = xs !! (n-2)
          last        = xs !! (n-1)
          mid         = take (n-4) (drop 2 xs)
      in [second, first] ++ mid ++ [last, penultimate]

-- Q16
produto :: Num a => [a] -> a
produto [x] = x
produto (x:xs) = x* produto xs

-- Q17 
ultimo :: [a] -> a -> [a]
ultimo xs n = xs ++ [n]

-- Q18
concatena :: [a] -> [a] -> [a]
concatena []     ys = ys
concatena (x:xs) ys = x : (concatena xs ys)

-- Q19
crivo :: [Int] -> [Int]
crivo [] = []
crivo (p:ps) = p : crivo [x | x <- ps, not (x `mod` p == 0)]

-- Q24
rotacoes :: [a] -> [[a]]
rotacoes xs =
   let n = length xs
   in [ drop i xs ++ take i xs | i <- [0..n-1] ]
   
-- Q25
supMed :: (Fractional a, Ord a) => [a] -> [a]
supMed xs =
   let m = (sum xs) / fromIntegral (length xs)
   in [x | x <- xs, x > m]
   
-- Q26
maximos :: Ord a => [(a,a)] -> [a]
maximos [] = []
maximos ((x,y):pares) = max x y : maximos pares

-- Q30
comprime :: Eq a => [a] -> [a]
comprime [] = []
comprime [x] = [x]
comprime (x:y:xs) = if x==y then comprime (y:xs) else [x] ++ comprime (y:xs)

-- Q31
empacota :: Eq a => [a] -> [[a]]
empacota [] = []
empacota [x] = [[x]]
empacota (x:y:xs)
   | x==y = 
      let (firstGroup : otherGroups) = empacota (y:xs)
      in (x : firstGroup) : otherGroups
   | otherwise = [x] : empacota (y:xs)

-- Q33
replique :: [a] -> Int -> [a]
replique xs 0 = []
replique [] _ = []
replique (x:xs) n = x : replique [x] (n-1) ++ replique xs n

-- Q37 
combinacoes :: Int -> [a] -> [[a]]
combinacoes 0 _ = [[]]
combinacoes _ [] = []
combinacoes n (x:xs) = com_x ++ sem_x
   where
      subCombinacoes = combinacoes (n-1) xs
      com_x = [ (x:sub) | sub <- subCombinacoes ]
      sem_x = combinacoes n xs

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
