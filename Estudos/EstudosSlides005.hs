takeV2 :: Int -> [a] -> [a]
takeV2 n _ | n <= 0 = []
takeV2 _ [] = []
takeV2 n (x:xs) = x : take (n-1) xs

dropV2 :: Int -> [a] -> [a]
dropV2 n xs | n <= 0 = xs
dropV2 _ [] = []
dropV2 n (_:xs) = drop (n-1) xs

splitAtV2 :: Int -> [a] -> ([a], [a])
splitAtV2 n xs | n <= 0 = ([], xs)
splitAtV2 _ [] = ([],[])
splitAtV2 n (x:xs) = (x:xs', xs'')
   where
      (xs',xs'') = splitAtV2 (n-1) xs
      
init2 :: [a] -> [a]
init2 [_] = []
init2 (x:xs) = [x] ++ init2 xs

last2 :: [a] -> a
last2 [x] = x 
last2 (_:xs) = last2 xs

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
-- elem2 n [x] = if x == n then True else False
elem2 n (x:xs) = if x == n then True else elem2 n xs
-- OU
-- elem2 _ [] = False
-- elem2 x (y:ys) = x==y || elem2 x ys (se o primeiro é false ele tem q verificar o segundo)

-- infixl 9 !! -- posição prefixa
-- (!!) :: [a] -> Int -> a
-- (x:_) !! 0 = x
-- (_:xs) !! n = xs !! (n-1)

-- infixr 5 ++
-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x:xs) ++ ys = x (xs++ys)

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = xs ++ concat xss

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

zip2 :: [a] -> [b] -> [(a,b)]
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys
zip2 _ _ = []

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 xs = unzip2' xs [] []
   where
      unzip2' ((a,b):xs) as bs = unzip2' xs (a:as) (b:bs)
      unzip2' _ as bs = (reverse as, reverse bs)

teste :: (String,Char,Int,Double) -> Double
teste info = case info of
           (_,_,idade,peso)   | idade < 18 -> 2*peso
                              | idade < 21 -> 3*peso
           (_,'F',_,peso)                  -> peso
           (_,'M',idade,peso) | idade < 40 -> peso + 10
                              | otherwise  -> 0.9*peso
