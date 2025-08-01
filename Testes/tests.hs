último :: [Integer] -> Integer
último [x] = x
último (x:xs) = último xs

countElements :: [Integer] -> Integer
countElements [] = 0
countElements (_:xs) = 1 + countElements xs

sumElements :: [Integer] -> Integer
sumElements [x] = x
sumElements (x:xs) = x + sumElements xs

parity :: [Integer] -> (Integer,Integer)
parity [] = (0,0)
parity (x:xs) =
  let (nEven, nOdd) = parity xs in
  if even(x)
  then (nEven+1,nOdd)
  else (nEven,nOdd+1)
  

