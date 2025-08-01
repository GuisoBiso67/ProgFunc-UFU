-- estudos slides 1

areaQuad :: Int -> Int -> Int
areaQuad a b = a*b

areaRet :: Int -> Int -> Int
areaRet a b = a*b

areaCirc :: Double -> Double
areaCirc r = pi*r*r -- pi é "constante" em haskell;

areaTri :: Double -> Double -> Double
areaTri a h = (a*h)/2

areaTrap :: Double -> Double -> Double -> Double
areaTrap a b h = ((a+b)*h)/2

-- condições if

maior :: Int -> Int -> Int
maior x y = if x >= y then x else y-- condição (if deve ser seguido do then e else);
            --then x -- true;
            --else y -- false;

soma :: Int -> Int -> Int
soma x y = x + (if y<0 then -y else y)

bissexto :: Int -> Bool
bissexto x = if mod x 4 /= 0
             then False
             else if mod x 100 /= 0
                  then True
                  else if mod x 400 == 0
                       then True
                       else False
                       
nomeMes :: Int -> String
nomeMes m = if m == 1 then "Jan"
            else if m == 2 then "Feb"
            else if m == 3 then "Mar"
            else if m == 4 then "Apr"
            else if m == 5 then "May"
            else if m == 6 then "Jun"
            else if m == 7 then "Jul"
            else if m == 8 then "Aug"
            else if m == 9 then "Sep"
            else if m == 10 then "Oct"
            else if m == 11 then "Nov"
            else if m == 12 then "Dec"
            else "Invalid Number"

nomeMes_v2 :: Int -> String
nomeMes_v2 m 
   | m == 1 = "Jan"
   | m == 2 = "Feb"
   | m == 3 = "Mar"
   | m == 4 = "Apr"
   | m == 5 = "May"
   | m == 6 = "Jun"
   | m == 7 = "Jul"
   | m == 8 = "Aug"
   | m == 9 = "Sep"
   | m == 10 = "Oct"
   | m == 11 = "Nov"
   | m == 12 = "Dec"
   
bissexto_v2 :: Int -> Bool
bissexto_v2 x
   | mod x 4 /= 0 = False
   | mod x 400 == 0 = True
   | mod x 100 == 0 = False
   | otherwise = True
   
maiorDeTres a b c
   | a >= b && a >= c = a
   | b >= c = b
   | True = c -- alternativa para otherwise

calcula :: Int -> Int -> Char -> Int
calcula a b op = case op of
   '+' -> a + b
   '-' -> a - b
   '*' -> a * b
   '/' -> a `div` b
   '%' -> a `mod` b
   _ -> 0 -- op invalido

principal a b op = 
   let resp = calcula a b op
   in show a ++ " " ++ [op] ++ " " ++ show b ++ " = " ++ show resp


abrev :: String -> String
abrev nome = case nome of
   "Guilherme Cotrim" -> "G. C."
   "Policarpo Quaresma" -> "P. Q."
   "Rodrigo Serra" -> "R. S."
   _ -> "nome nao cadastrado"


