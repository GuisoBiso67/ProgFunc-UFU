-- ESTUDOS SLIDES 2

-- fatorial
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial x = x * fatorial(x-1)

-- ultimo elemento de uma sequencia
ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo xs 

-- contar numero de elementos de uma sequencia
elements :: [Integer] -> Integer
elements [x] = 1 -- ou [] = 0
elements (x:xs) = 1 + elements xs

--soma dos elementos de uma sequencia
sumOfElements :: [Integer] -> Integer
sumOfElements [x] = x
sumOfElements (x:xs) = x + sumOfElements xs

--inverter a ordem de uma sequencia
reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

-- LeetCode #70 - Climbing Stairs
stairs :: Int -> Int
stairs 1 = 1
stairs 2 = 2
stairs x = stairs (x-1) + stairs (x-2)

-- LeetCode #231 - Power of Two
powerOfTwo :: Integer -> Bool
powerOfTwo 0 = False
powerOfTwo 1 = True
powerOfTwo x = if mod x 2 == 0 then powerOfTwo (div x 2) else False

pot2 :: Integer -> Bool
pot2 n
   | n == 1 = True
   | n < 1 || n `mod` 2 == 1 = False
   | otherwise = pot2 (n `div` 2)

-- contagem de paridade de uma sequencia
parity :: [Integer] -> (Integer, Integer)
parity [] = (0,0)
parity (x:xs) = 
   let (nPar, nImpar) = parity xs in
   if even x
   then (nPar+1,nImpar)
   else (nPar,nImpar+1)
   
-- encontrar picos de uma sequencia
picos :: [Integer] -> [Integer]
picos [] = []
picos [_] = []
picos [_,_] = []
picos (x:y:z:xs) 
   | x < y && y > z = y : picos (z:xs)
   | otherwise = picos (y:z:xs)
   
-- fibonacci 
fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib x 
   | x > 1 = fib (x-1) + fib (x-2)
   | otherwise = error "Argumento deve ser estritamente positivo"

-- :set +s = mostra gasto de memoria e tempo de execução
-- O(n):
fib2 :: Int -> Int
fib2 n
   | n < 1 = error "Argumento deve ser estritamente positivo"
   | otherwise = fib' n 0 1
   where
      fib' 1 a _ = a
      fib' k a b = fib' (k-1) b (a+b)

