-- Q5
eq2 :: Double -> Double -> Double -> (Double,Double)
eq2 a b c =
   let 
      delta = (b^2)-4*a*c
      x1 = ((-b) + sqrt delta) / (2*a)
      x2 = ((-b) - sqrt delta) / (2*a)
   in 
      if delta < 0 then error "Delta eh negativo" else (x1,x2)
      
-- Q6
mmc :: Integer -> Integer -> Integer
mmc a b
    | a == 0 || b == 0 = 0
    | otherwise        = (abs (a * b)) `div` (mdc a b) -- multiplica os dois e divide pelo q eh comum (mdc)

-- Q7
mdc :: Integer -> Integer -> Integer
mdc a b = mdcAux (abs a) (abs b)
 where
    mdcAux x 0 = x
    mdcAux x y = mdcAux y (x `mod` y) -- mdc de y e resto da divisao de x por y
    
-- Q8
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
