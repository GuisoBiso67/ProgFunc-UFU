import Data.Char

-- Q2. 'expande'
-- Divide uma lista em sublistas, onde cada sublista contém elementos que produzem o mesmo resultado para o predicado.
expande :: (a->Bool) -> [a] -> [[a]]
expande _ [] = [] -- Caso base: lista vazia retorna lista vazia.
expande p (x:xs) =
  let (ys,zs) = span (\y -> p y == p x) (x:xs) -- Usa 'span' para separar a lista em dois segmentos. O primeiro (ys) tem todos os elementos que produzem o mesmo resultado do primeiro elemento 'x'. O segundo (zs) é o resto.
  in
    ys : expande p zs -- Adiciona o primeiro segmento encontrado ('ys') e continua a recursão no restante da lista ('zs').
    
-- Q3. 'separaNumeros'
-- Usa a função 'expande' para separar uma string em segmentos de dígitos e não-dígitos.
separaNumeros :: String -> [String]
separaNumeros [] = [] -- Caso base: string vazia retorna lista vazia.
separaNumeros xs = expande isDigit xs -- Usa a função 'expande' com o predicado 'isDigit'.

-- Q4. 'until2'
-- Aplica uma função a um valor repetidamente até que um predicado seja verdadeiro.
until2 :: (a->Bool) -> (a->a) -> a -> a
until2 p f x = if p x then x else until2 p f (f x) -- Se o predicado 'p' for verdadeiro para 'x', retorna 'x'. Caso contrário, aplica a função 'f' a 'x' e chama 'until2' novamente.

-- Q7. 'dig2int'
-- Converte uma lista de dígitos em um número inteiro usando recursão.
dig2int :: [Integer] -> Integer
dig2int [] = 0 -- Caso base: lista vazia de dígitos retorna 0.
dig2int (x:xs) = x + 10 * dig2int xs -- O primeiro dígito é somado a 10 vezes o número inteiro do resto da lista.

-- Q7. 'dig2intFold'
-- Converte uma lista de dígitos em um número inteiro usando 'foldr'.
dig2intFold :: [Integer] -> Integer
dig2intFold = foldr (\x acc -> x + 10 * acc) 0 -- Usa 'foldr' para somar cada dígito ao acumulador, que é multiplicado por 10 em cada passo, começando de 0.

-- 'sufixosFold'
-- Cria uma lista de todos os sufixos de uma lista usando 'foldr'.
sufixosFold :: [a] -> [[a]]
sufixosFold = foldr (\x acc -> (x : head acc) : acc) [[]] -- 'foldr' constrói a lista de trás para frente. A cada passo, pega o elemento atual 'x', o adiciona ao primeiro elemento do acumulador ('head acc', que é o sufixo já construído) e adiciona a nova lista ao início do acumulador.

-- Q12. 'produtoImpares'
-- Calcula o produto de todos os números ímpares em uma lista de inteiros.
produtoImpares :: [Integer] -> Integer
produtoImpares [] = 0 -- Caso base: lista vazia retorna 0.
produtoImpares xs = 
  if listFilter == [] then 0 -- Se a lista filtrada estiver vazia (sem ímpares), retorna 0.
  else foldr (*) 1 listFilter -- Se houver ímpares, usa 'foldr' para multiplicá-los, com 1 como acumulador inicial.
  where
    listFilter = (filter (\x -> x`mod`2 /= 0) xs) -- Filtra a lista para manter apenas os números ímpares.
  
-- Q13. 'maiusculas'
-- Converte uma lista de strings em uma única string, com as letras em maiúsculas e separadas por espaços.
maiusculas :: [String] -> String
maiusculas [[]] = [] -- Caso base: lista com uma string vazia retorna string vazia.
maiusculas xs = map toUpper (foldr (\x acc -> x ++ " " ++ acc) [] xs) -- Usa 'foldr' para concatenar todas as strings da lista com espaços entre elas. Em seguida, 'map toUpper' converte todos os caracteres da string resultante para maiúsculas.

-- Q15. 'maior'
-- Encontra o maior elemento em uma lista de inteiros usando 'foldl'.
maior :: [Int] -> Int
maior [] = error "Lista vazia!!" -- Trata o erro para listas vazias.
maior (x:xs) = foldl (\a e -> if a>=e then a else e) x xs -- Usa 'foldl' para percorrer a lista da esquerda para a direita, comparando o acumulador ('a') com o próximo elemento ('e') e mantendo o maior valor.

-- Q16. 'contaSe'
-- Conta quantos elementos em uma lista satisfazem um predicado.
contaSe :: (a -> Bool) -> [a] -> Int
contaSe _ [a] = 0 -- Caso base: lista com um único elemento. Não é ideal, pois uma lista de um elemento pode satisfazer o predicado, mas segue o código original.
contaSe p xs = foldl (\acc x -> if p x then acc+1 else acc+0) 0 xs -- Usa 'foldl' para percorrer a lista e incrementar um contador ('acc') se o predicado ('p') for verdadeiro para o elemento atual ('x').

-- Q17. 'mapComFoldl'
-- Reimplementa a função 'map' usando 'foldl'.
mapComFoldl :: (a -> b) -> [a] -> [b]
mapComFoldl _ [] = [] -- Caso base: lista vazia.
mapComFoldl f xs = reverse (foldl (\li x -> (f x) : li) [] xs) -- Usa 'foldl' para construir uma nova lista, mas de forma reversa. Depois, usa 'reverse' para corrigir a ordem.
---------- NAO FIZ ESSES ----------------------------------------------
-- Q18. 'removeDuplicatas'
-- Remove elementos duplicados de uma lista, mantendo a ordem original, usando 'foldl'.
removeDuplicatas :: Eq a => [a] -> [a]
removeDuplicatas = foldl (\acc x -> if x `elem` acc then acc else x : acc) [] -- 'foldl' percorre a lista. Para cada elemento 'x', ele verifica se 'x' já está no acumulador ('acc'). Se não estiver, ele adiciona 'x' ao início do acumulador.

-- Q19. 'media'
-- Calcula a média de uma lista de 'Double's.
media :: [Double] -> Double
media xs =
  let
    (soma, contagem) = foldl (\(s, c) x -> (s + x, c + 1)) (0.0, 0.0) xs -- Usa 'foldl' para percorrer a lista e acumular a soma e a contagem simultaneamente em uma tupla.
  in
    if contagem == 0 then 0.0 -- Lida com o caso de lista vazia para evitar divisão por zero.
    else soma / contagem -- Calcula a média.
      
-- Q20. 'regioes'
-- Calcula o número de regiões formadas por 'n' retas em um plano.
regioes :: Int -> Int
regioes n = (n*n + n + 2) `div` 2 -- Aplica a fórmula matemática para o problema.

-- Q21. 'argMax'
-- Encontra o argumento 'x' no intervalo [0..n] que maximiza o resultado da função 'g'.
argMax :: (Integer -> Integer) -> Integer -> Integer
argMax g n
  | n < 0     = error "Intervalo inválido: n não pode ser negativo" -- Trata o caso de entrada inválida.
  | otherwise = snd (maximum (map (\x -> (g x, x)) [0..n])) -- Mapeia o intervalo para uma lista de tuplas (g x, x), encontra a tupla com o maior primeiro elemento e retorna o segundo elemento (o 'x').

-- Q22. 'concat''
-- Concatena uma lista de listas usando uma função auxiliar recursiva.
concat' :: [[a]] -> [a]
concat' listas = go listas [] -- Inicia a função auxiliar `go` com a lista de listas e um acumulador vazio.
  where
    go [] acc = acc -- Caso base: quando a lista de listas estiver vazia, retorna o acumulador.
    go (xs:xss) acc = go xss (acc ++ xs) -- Concatena a lista atual 'xs' ao acumulador 'acc' e chama 'go' recursivamente com o resto das listas.

-- Q23. 'replicador'
-- Para cada número 'n' em uma lista, cria uma lista com 'n' cópias de 'n' e concatena tudo.
replicador :: [Int] -> [Int]
replicador = concatMap (\n -> replicate n n) -- Usa 'concatMap' que é uma combinação de 'map' e 'concat'. Para cada 'n', 'replicate n n' cria uma lista com 'n' cópias de 'n'. 'concatMap' junta todas essas listas em uma só.

-- Q24. 'inclinacao'
-- Calcula a inclinação (derivada) de uma função em um ponto usando aproximação.
inclinacao :: (Double -> Double) -> (Double -> Double)
inclinacao f = \x -> (f (x + h) - f x) / h -- A função anônima 'lambda' recebe 'x' e aplica a fórmula de inclinação.
  where
    h = 1e-7 -- Define um valor pequeno para 'h' para uma boa aproximação.

-- Q25. 'negativos'
-- Usa list comprehension para retornar apenas os números negativos de uma lista.
negativos :: [Integer] -> [Integer]
negativos xs = [x | x <- xs, (x<0)] -- 'x <- xs' itera sobre a lista. '(x<0)' é o filtro.

-- Q26. 'uniao' e 'intersecao'
-- Implementam a união e interseção de duas listas usando list comprehension.
uniao :: Eq t => [t] -> [t] -> [t]
uniao as bs = as ++ [b | b <- bs, not (elem b as)] -- A união é a primeira lista ('as') mais todos os elementos da segunda lista ('bs') que não estão em 'as'.

intersecao :: Eq t => [t] -> [t] -> [t]
intersecao as bs = [b | b <- bs, (elem b as)] -- A interseção são os elementos da segunda lista ('bs') que também estão na primeira ('as').

-- Q27. 'distancias'
-- Calcula a distância euclidiana de cada ponto (x,y) em uma lista.
distancias :: [(Float,Float)] -> [Float]
distancias [] = [] -- Caso base: lista de tuplas vazia retorna lista vazia.
distancias xys = [(sqrt (x^2 + y^2)) | (x,y) <- xys] -- Usa list comprehension para iterar sobre a lista de tuplas e aplicar a fórmula da distância.

-- Q28. 'posicoes'
-- Encontra todos os índices de um valor específico em uma lista.
posicoes :: Eq a => a -> [a] -> [Int]
posicoes valor xs = [i | (x, i) <- zip xs [0..], x == valor] -- Usa 'zip' para emparelhar cada elemento 'x' com seu índice 'i'. A list comprehension filtra os pares onde o elemento 'x' é igual ao 'valor' e retorna apenas o índice 'i'.

-- Q29. 'fatorialIntermediarios' e 'fatorial'
-- 'fatorialIntermediarios' gera uma lista de fatoriais de 1 até n. 'fatorial' usa essa lista para retornar o fatorial de n.
fatorialIntermediarios :: Integer -> [Integer]
fatorialIntermediarios n = [product [1..k] | k <- [1..n]] -- Usa list comprehension para calcular o produto de 1 a k para cada k no intervalo.

fatorial :: Integer -> Integer
fatorial n = last (fatorialIntermediarios n) -- Apenas pega o último elemento da lista de fatoriais intermediários.

-- Q31. 'transposta'
-- Transpõe uma matriz (lista de listas).
transposta :: [[a]] -> [[a]]
transposta [] = [] -- Caso base: matriz vazia.
transposta ([]:_) = [] -- Caso de uma lista de listas vazias.
transposta m = [[linha !! i | linha <- m] | i <- [0..length (head m) - 1]] -- Usa list comprehension para iterar sobre os índices 'i' das colunas e, para cada índice, cria uma nova linha com os elementos daquela coluna.

-- Q32. 'separaNomes'
-- Separa uma lista de nomes completos em duas listas: uma com os primeiros nomes e outra com os últimos.
separaNomes :: [String] -> ([String], [String])
separaNomes nomes = unzip (map separa nomes) -- Mapeia a função 'separa' para cada nome e depois usa 'unzip' para separar a lista de tuplas em duas listas.
  where
    separa nomeCompleto = (head palavras, last palavras) -- 'separa' divide o nome em palavras, pega a primeira e a última, e retorna uma tupla.
      where
        palavras = words nomeCompleto -- Divide a string de nome em uma lista de palavras.
