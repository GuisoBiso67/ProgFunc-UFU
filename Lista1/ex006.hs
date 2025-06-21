mmc2 :: Integer -> Integer -> Integer
mmc2 a b = abs (a*b) `div` gcd a b

mmc3 :: Integer -> Integer -> Integer -> Integer
mmc3 a b c = mmc2 (mmc2 a b) c

