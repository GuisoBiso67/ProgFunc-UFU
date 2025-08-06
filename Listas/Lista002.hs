import Data.Char

-- Q1
min2 :: Int -> Int -> Int
minTres :: Int -> Int -> Int -> Int
min2 a b 
   | a < b = a
   | otherwise = b

minTres a b c
   | a < b && a < c = a
   | b < c = b
   | otherwise = c
   
-- Q2

maiusculas :: String -> String
maiusculas [] = []
maiusculas (x:xs) = 
   let 
      charM = toUpper x
      restoM = maiusculas xs
   in
      charM : restoM

-- Q6
numeroRaizes :: Double -> Double -> Double -> Integer
numeroRaizes a b c
   | a==0 = error " -a- deve ser diferente de zero"
   | (b^2)-4*a*c == 0 = 1
   | otherwise = 2
   
-- Q8
entre :: Integer -> Integer -> Integer -> Bool
entre m n p = (m <= n && n <= p) || (p <= n && n <= m)

numeroDoMeio :: Integer -> Integer -> Integer -> Integer
numeroDoMeio a b c
   | (b <= a && a <= c) || (c <= a && a <= b) = a
   | (a <= b && b <= c) || (c <= b && b <= a) = b
   | otherwise = c
   
-- Q10
maxTresOc :: Int -> Int -> Int -> (Int, Int)
maxTresOc a b c
    | a == b && b == c = (a, 3)
    | a == b && a > c  = (a, 2)
    | a == c && a > b  = (a, 2)
    | b == c && b > a  = (b, 2)
    | a > b && a > c   = (a, 1)
    | b > a && b > c   = (b, 1)
    | otherwise        = (c, 1)
    
-- Q11
ordenaTripla :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
ordenaTripla (a,b,c) =
   let
      menor = min a (min b c)
      maior = max a (max b c)
      meio = (a+b+c) - menor - maior
   in (menor, meio, maior)

-- Q12
reta :: Double -> Double -> (Double,Double)
reta a b = ((-b)/a , 0)

-- Q13
intersecta :: (Double,Double) -> (Double,Double) -> (Double,Double)
intersecta (m1,c1) (m2,c2) =
   let 
      x = (c2-c1)/(m1-m2)
      y = m1*x+c1
   in 
      (x,y)

-- Q15
segundos :: Int -> Int -> Int -> Int
segundos h m s = 3600*h + 60*m + s

-- Q16
bissexto :: Int -> Bool
bissexto x
   | x `mod` 400 == 0 = True
   | x `mod` 4 == 0 && x `mod` 100 /= 0 = True
   | otherwise = False

-- Q18
f :: (Num a, Ord a) => a -> a
f x
  | x <= 0    = 0
  | otherwise = x

-- Q25 (verificar se a palavra eh pequena demais)
--tema :: String -> (String,String)
--tema xs = (reverse (drop 2 strRev), [head (reverse (take 2 strRev))] )
   --where
      --strRev = reverse xs
