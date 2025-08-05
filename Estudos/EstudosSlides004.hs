ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

picos :: Ord a => [a] -> [a]
picos [] = []
picos [_] = []
picos [_,_] = []
picos (x:y:z:xs)
   | x<y && y>z = y : picos (z:xs)
   | otherwise = picos (y:z:xs)


