module Main (main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

celsius :: Double -> Double
celsius f = (5/9)*(f-32)

pesoIdeal :: Char -> Double -> Double
pesoIdeal c h
  | c == 'M'  = (62.1*h)-44.7
  | otherwise = (72.7*h)-58
  
situacao :: Double -> Double -> Double -> String
situacao n1 n2 n3
  | media < 30 = "Reprovado!"
  | media <= 59 = "Substitutiva"
  | otherwise  = "Aprovado!"
    where
      media = (n1+n2+n3)

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  putStr "Digite um valor em Fahrenheit: "
  f <- getLine :: IO Double
  putStrLn (f ++ " em Celsius eh " ++ (show (celsius(read f :: Double))) ++ "C")
  
  putStr "Voce eh homem ou mulher? (M/H): "
  s <- getLine
  putStr "Digite sua altura em metros: "
  h <- getLine
  putStrLn ("Seu peso ideal eh " ++ (show (pesoIdeal (head s) (read h :: Double))))
  
  putStr "Nota 1: "
  n1 <- readLn :: IO Double
  putStr "Nota 2: "
  n2 <- readLn
  putStr "Nota 3: "
  n3 <- readLn
  putStrLn ("Sua situacao: " ++ situacao n1 n2 n3)
  
