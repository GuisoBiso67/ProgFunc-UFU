import Data.Char(isUpper, isLower, isDigit, toUpper)
-- FUNÇÕES DE ALTA ORDEM

-- ///////////////////
-- Função de Alta Ordem --
somatorio :: (Fractional a, Ord a) => a -> a -> (a -> a) -> (a -> a) -> a
somatorio a b parcela proximo
   | a > b = 0
   | otherwise = (parcela a) + somatorio (proximo a) b parcela proximo

somaInts a b = somatorio a b id succ

somaCubos a b = somatorio a b cubo succ
   where
      cubo a = a^3
{-
somaPi a b = somatorio a b fracao prox
   where
      fracao a = 1 / (a*(a+2))
      prox a = a + 4
-}
-- ///////////////////

-- ///////////////////
-- Funções Lambda (funções anônimas)

somaPi a b = somatorio a b (\a -> recip (a*(a+2))) (\a -> a+4)
-- \parametro1 parametro2 ... -> corpo da função

criaIncrementador :: Int -> (Int -> Int)
criaIncrementador n = (\x -> x + n)

aplicaCond :: (a->Bool) -> (a->a) -> (a->a)
aplicaCond pred f = (\x -> if pred x then f x else x)
-- ///////////////////

-- ///////////////////
-- Composição de Funções (f o g)(x) = f(g(x)) --
compoe :: (b->c)->(a->b)->(a->c)
compoe f g = \x -> f (g x)
-- soma1edobra = compoe (\x -> 2*x) succ -- o mesmo do de baixo, mas usando a função compoe;
-- soma1edobra = (\x -> 2*x) . succ -- o ponto é para representar a bola em f o g;
-- a função flip inverte a ordem dos argumentos de uma função;
-- ///////////////////

-- ///////////////////
-- Aplicação Parcial de Função --
-- a aplicação parcial permite que você fixe alguns argumentos de uma função;
{-
soma :: Int -> Int -> Int
soma x y = x+y
-}
-- versao parcialmente aplicada da função soma com o 1 fixado como 1º argumento;
somaUm :: Int -> Int
somaUm = soma 1
-- transformando soma em expressão lambda
soma = \x y -> x+y
-- ///////////////////

-- ///////////////////
-- Seções --
-- sintaxe especial para a aplicação parcial de um operador infixo a um único argumento
triplo :: Int -> Int
triplo = (*3)

cincoPor = (/) 5 -- fixa numerador
sobreCinco = (/ 5) -- fixa denominador
temPera :: [String] -> Bool
temPera = ("pera" `elem`)
ehBaga = (`elem` ["morango", "amora", "framboesa", "mirtilo"])
-- ///////////////////

-- ///////////////////
-- Funções de Alta Ordem sobre Listas --
-- mapeamento de uma função sobre uma lista
-- forma bruta:
alteraNLista :: (Num a) => a -> [a] -> [a]
alteraNLista fator [] = []
alteraNLista fator (x:xs) = (x * fator) : alteraNLista fator xs
-- função map (que ja é padrão do haskell) = map (funçao) [lista]
mapear :: (a->b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs
valoresAbs xs = mapear abs xs

-- função filter = filter (condição) [lista]
filtro :: (a -> Bool) -> [a] -> [a]
filtro pred [] = []
filtro pred (x:xs)
   | pred x    = x : filter pred xs
   | otherwise = filter pred xs
   
-- juntando as duas funções:
dobraEfiltra = filter (>=10) . map (*2)
quadImpPos = map (^2) . filter (\x -> x > 0 && odd x)

-- função "sum" recebe lista e retorna soma de todos;
-- função "product" recebe lista e retorna a multiplicação de todos eles;
-- função "concat" recebe várias listas e concatena todas;
folDR :: (a->a->a) -> a -> [a] -> a
folDR f v [] = v
folDR f v (x:xs) = x `f` (folDR f v xs)
-- foldr (operador) (elemento neutro) [lista]
-- foldr é associativo a direita
-- foldl é associativo a esquerda (menos natural)
-- foldr1 e foldl1 são para listas que você sabe que nunca serão vazias

-- Outras Funções de Alta Ordem sobre Listas
-- takeWhile (percorre e pega os elementos de uma lista até que a condição seja falsa)
-- dropWhile (percorre e retira os elementos de uma lista até que a condição seja falsa)
-- zipWith (aplica uma função binaria aos elementos correspondentes de duas listas)
-- repeat (repete infinitamente uma lista com o mesmo elemento)
-- replicate (replica n vezes um mesmo elemento n em uma lista)
-- cycle (recebe uma lista base e repete os elementos infinitamente em outro lista)
-- iterate (recebe uma função que usa de argumento o elemento anterior para gerar o proximo)
-- any (verifica de algum elemento da lista satisfaz uma condição)
-- all (verifica de todos os elementos satisfazem uma condição)

-- Notação de Zermelo-Fraenkel
-- compreensão de listas:
-- [x^2 | x <- [1..5] (aplica o x^2 em todos os elementos da lista)
-- é possível multiplos geradores: [(x,y) | x <- [1,2,3], y <- [4,5]]
-- os geradores mais a direita podem depender dos geradores anteriores:
-- EX: [(x,y) | x <- [1..3], y <- [x..3]] gera [ (1,1), (1,2), (1,3), (2,2), (2,3), (3,3) ]. (x,y) se y >= x;
--Primeiro gerador: x <- [1..3], x assume os valores 1, 2 e 3.
--Segundo gerador: y <- [x..3], para cada valor de x, y percorre a lista que começa em x e vai até 3.
--Ou seja:
--Quando x = 1, y <- [1..3] → y = 1, 2, 3
--Quando x = 2, y <- [2..3] → y = 2, 3
--Quando x = 3, y <- [3..3] → y = 3

-- é possível usar para restringir os valores produzidos pelo gerador: [x | x <- [1..10], even x], retorna uma lista de pares entre 1 e 10

-- Aninhamento de Compreensão de Listas
-- usados para pares ordenados que vem de listas infinitas

-- Variáveis Locais com let
-- ex: [x | i <- [1..10], let x = i*i, x > 20]
-- ex: let pares = [(i, j) | i <- [1..], let k = i*i, j <- [1..k]] -- saída infinita

-- Compreensão de listas e map e filter
xs = [(c1,c2) | c1 <- "Nao vou a escola hoje.",
                c2 <- "A segunda string.",
                isLower c1 && isUpper c2 && toUpper c1 == c2]

ys = filter (\ par -> toUpper (fst par) == (snd par))
            (concat
               (map (\ c1 -> map (\ c2 -> (c1, c2))
                                 (filter isUpper "A segunda string."))
                    (filter isLower "A primeira string.")))

-- Aplicações de Função com $
-- $ diminui os parenteses numa função e é associativo a esquerda
-- sum (map sqrt [1..130]) ou sum $ map sqrt [1..130]


