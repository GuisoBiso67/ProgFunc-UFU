import Data.Char

--Q2
expande :: (a->Bool) -> [a] -> [[a]]
expande _ [] = []
expande p (x:xs) =
  let (ys,zs) = span (\y -> p y == p x) (x:xs)
  in
    ys : expande p zs
    
--Q3
separaNumeros :: String -> [String]
separaNumeros [] = []
separaNumeros xs = expande isDigit xs

--Q4
until2 :: (a->Bool) -> (a->a) -> a -> a
until2 p f x = if p x then x else until2 p f (f x)

--Q7
dig2int :: [Integer] -> Integer
dig2int [] = 0
dig2int (x:xs) = x + 10 * dig2int xs

dig2intFold :: [Integer] -> Integer
dig2intFold = foldr (\x acc -> x + 10 * acc) 0

sufixosFold :: [a] -> [[a]]
sufixosFold = foldr (\x acc -> (x : head acc) : acc) [[]]

--Q12
produtoImpares :: [Integer] -> Integer
produtoImpares [] = 0
produtoImpares xs = 
  if listFilter == [] then 0
  else foldr (*) 1 listFilter
  where
    listFilter = (filter (\x -> x`mod`2 /= 0) xs)
  
--Q13
maiusculas :: [String] -> String
maiusculas [[]] = []
maiusculas xs = map toUpper (foldr (\x acc -> x ++ " " ++ acc) [] xs)

--Q15
maior :: [Int] -> Int
maior [] = error "Lista vazia!!"
maior (x:xs) = foldl (\a e -> if a>=e then a else e) x xs

--Q16
contaSe :: (a -> Bool) -> [a] -> Int
contaSe _ [a] = 0
contaSe p xs = foldl (\acc x -> if p x then acc+1 else acc+0) 0 xs

--Q17
mapComFoldl :: (a -> b) -> [a] -> [b]
mapComFoldl _ [] = []
mapComFoldl f xs = reverse (foldl (\li x -> (f x) : li) [] xs)
---------- NAO FIZ ESSES ----------------------------------------------
--Q18
removeDuplicatas :: Eq a => [a] -> [a]
removeDuplicatas = foldl (\acc x -> if x `elem` acc then acc else x : acc) []

--Q19
media :: [Double] -> Double
media xs =
  let
    (soma, contagem) = foldl (\(s, c) x -> (s + x, c + 1)) (0.0, 0.0) xs
  in
    if contagem == 0 then 0.0
    else soma / contagem
      
--Q20
regioes :: Int -> Int
regioes n = (n*n + n + 2) `div` 2

--Q21
argMax :: (Integer -> Integer) -> Integer -> Integer
argMax g n
  | n < 0     = error "Intervalo inválido: n não pode ser negativo"
  | otherwise = snd (maximum (map (\x -> (g x, x)) [0..n]))

--Q22
concat' :: [[a]] -> [a]
concat' listas = go listas []
  where
    go [] acc = acc
    go (xs:xss) acc = go xss (acc ++ xs)

--Q23
replicador :: [Int] -> [Int]
replicador = concatMap (\n -> replicate n n)

--Q24
inclinacao :: (Double -> Double) -> (Double -> Double)
inclinacao f = \x -> (f (x + h) - f x) / h
  where
    h = 1e-7 -- Um valor pequeno para h

--Q25
--(++) . reverse
------------------------ PARTE 2 ---------------------------------
--Q18
negativos :: [Integer] -> [Integer]
negativos xs = [x | x <- xs, (x<0)]

--Q19
uniao :: Eq t => [t] -> [t] -> [t]
uniao as bs = as ++ [b | b <- bs, not (elem b as)]

intersecao :: Eq t => [t] -> [t] -> [t]
intersecao as bs = [b | b <- bs, (elem b as)]

--Q20
distancias :: [(Float,Float)] -> [Float]
distancias [] = []
distancias xys = [(sqrt (x^2 + y^2)) | (x,y) <- xys]

--Q21
posicoes :: Eq a => a -> [a] -> [Int]
posicoes valor xs = [i | (x, i) <- zip xs [0..], x == valor]

--Q23
fatorialIntermediarios :: Integer -> [Integer]
fatorialIntermediarios n = [product [1..k] | k <- [1..n]]

fatorial :: Integer -> Integer
fatorial n = last (fatorialIntermediarios n)

--Q24
-- [(x, y) | x <- [1..4], y <- [5..8], x + y > 10]

--Q25
sequenciaGeometrica :: Double -> Double -> Int -> [Double]
sequenciaGeometrica a r n = [a * r^k | k <- [0..fromIntegral n - 1]]

--Q26
-- a) map (+3) xs === [x + 3 | x <- xs]
-- b) filter (>7) xs === [x | x <- xs, x > 7]
-- c) concat (map (\x -> map (\y -> (x,y)) ys) xs) === [(x, y) | x <- xs, y <- ys]
-- d) filter (>3) (map (\(x,y) -> x+y) xys) === [x + y | (x, y) <- xys, x + y > 3]

--Q27
transposta :: [[a]] -> [[a]]
transposta [] = []
transposta ([]:_) = []
transposta m = [[linha !! i | linha <- m] | i <- [0..length (head m) - 1]]

--Q28
separaNomes :: [String] -> ([String], [String])
separaNomes nomes = unzip (map separa nomes)
  where
    separa nomeCompleto = (head palavras, last palavras)
      where
        palavras = words nomeCompleto
