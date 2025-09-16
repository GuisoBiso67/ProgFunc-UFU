import Data.List
import Data.Ord
import Data.Char

-- Q1. 'segmentos'
-- Divide uma lista em sublistas com base em um predicado.
segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos p = filter (p . head) . groupBy (\x y -> p x == p y) -- Agrupa os elementos que têm o mesmo resultado para o predicado 'p' e filtra os grupos que começam com um elemento que satisfaz 'p'.

-- Q1. 'segmentos2'
-- Outra implementação da função 'segmentos', mais explícita.
segmentos2 :: (a -> Bool) -> [a] -> [[a]]
segmentos2 p [] = [] -- Caso base: lista vazia não tem segmentos.
segmentos2 p (x:xs)
  -- Se o primeiro elemento satisfaz o predicado...
  | p x       = let (segment, rest) = span p (x:xs) -- Usa 'span' para obter o primeiro segmento que satisfaz o predicado.
                in segment : segmentos2 p rest -- Adiciona o segmento à lista e continua a recursão no restante da lista.
  -- Se o primeiro elemento não satisfaz o predicado...
  | otherwise = segmentos2 p xs -- ...pula o elemento e continua a busca por segmentos no resto da lista.
  
-- Q1. a) 'linhas'
-- Usa 'segmentos' para dividir uma string em uma lista de linhas.
linhas :: String -> [String]
linhas xs = segmentos (/= '\n') xs -- O predicado é "não ser uma quebra de linha".

-- Q1. b) 'palavras'
-- Usa 'segmentos' para dividir uma string em uma lista de palavras.
palavras :: String -> [String]
palavras xs = segmentos (not . isSpace) xs -- O predicado é "não ser um caractere de espaço".
isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r'] -- Define o que é um caractere de espaço.

-- Q2. a) 'foldn' e 'replica'
-- 'foldn' aplica uma função a um valor 'n' vezes. 'replica' usa 'foldn' para criar uma lista com 'n' cópias de um valor.
foldn :: (a->a)->a->Int->a
foldn f e 0 = e
foldn f e n = f (foldn f e (n-1))

replica :: Int -> a -> [a]
replica n v = foldn (v:) [] n

-- Q2. b) 'dupS'
-- Duplica cada caractere em uma string.
dupS :: String -> String
dupS str = concat (map (\c -> replica 2 c) str) -- Mapeia a função 'replica 2' para cada caractere e, em seguida, concatena todas as pequenas listas resultantes em uma única string.

-- Q3. 'prefixo' e 'prefixoR'
-- Ambas as funções filtram uma lista de strings para manter apenas aquelas que começam com um determinado prefixo.
prefixo :: String -> [String] -> [String]
prefixo pre telefs = filter (concorda pre) telefs -- Usa 'filter' com a função auxiliar 'concorda'.
  where concorda :: String -> String -> Bool
        concorda [] _ = True -- Caso base: se o prefixo é vazio, sempre concorda.
        concorda (x:xs) (y:ys) = (x==y) && (concorda xs ys) -- Compara os primeiros caracteres e, se forem iguais, continua a verificação recursivamente com o resto das strings.
        concorda (x:xs) [] = False -- Se o prefixo for maior que a string, não concorda.
        
prefixoR :: String -> [String] -> [String]
prefixoR [] _ = []
prefixoR _ [] = []
prefixoR pre tele
  | concorda pre (head tele) = (head tele) : prefixoR pre (tail tele) -- Se o primeiro elemento da lista concorda com o prefixo, ele é adicionado e a recursão continua.
  | otherwise = prefixoR pre (tail tele) -- Caso contrário, a recursão continua na próxima lista sem adicionar o elemento atual.

-- Q4. 'map''
-- Reimplementa 'map' usando 'foldr'.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) [] -- 'foldr' percorre a lista e, para cada elemento 'x', aplica a função 'f' e o adiciona ao acumulador ('acc').

-- Q5. a) 'pertence'
-- Verifica se um elemento pertence a uma lista usando 'foldr'.
pertence :: Eq a => a -> [a] -> Bool
pertence v = foldr (\x acc -> (x == v) || acc) False -- 'foldr' percorre a lista e o acumulador ('acc') armazena se o elemento já foi encontrado.

-- Q5. b) 'remdups'
-- Remove duplicatas de uma lista usando 'foldr'.
remdups :: Eq a => [a] -> [a]
remdups = foldr (\x acc -> if null acc || x /= head acc then x : acc else acc) [] -- 'foldr' constrói a lista do final para o início, verificando e adicionando apenas elementos únicos.

-- Q6. a) 'todos' e 'positivos'
-- 'todos' verifica se todos os elementos de uma lista satisfazem um predicado.
todos :: (a -> Bool) -> [a] -> Bool
todos p = foldr (\x acc -> p x && acc) True -- Usa 'foldr' com o operador lógico '&&'. O acumulador começa com 'True'.

positivos :: [Int] -> Bool
positivos = todos (>0) -- Usa 'todos' para checar se todos os números são maiores que zero.

-- Q6. b) 'algum' e 'membro'
-- 'algum' verifica se pelo menos um elemento satisfaz um predicado.
algum :: (a -> Bool) -> [a] -> Bool
algum p = foldr (\x acc -> p x || acc) False -- Usa 'foldr' com o operador lógico '||'. O acumulador começa com 'False'.

membro :: Eq a => a -> [a] -> Bool
membro v = algum (== v) -- Usa 'algum' para checar se o valor 'v' é membro da lista.

-- Q7. 'separa'
-- Divide uma lista em duas, alternando os elementos entre elas.
separa :: [a] -> ([a], [a])
separa = foldr (\x (l, r) -> (x:r, l)) ([], []) -- 'foldr' processa a lista de trás para a frente e alterna a adição de elementos entre as duas listas da tupla.

---------------------------- PARTE 2 ------------------------------
---------------------------- PARTE 2 ------------------------------
---------------------------- PARTE 2 ------------------------------

-- Q1. Tipos e funções para Viagem
-- Define os tipos de dados para representar diferentes classes de viagem.
data ClasseTrem = Primeira | Segunda deriving (Show, Eq)
data ClasseAviao = Executiva | Economica deriving (Show, Eq)

-- O tipo de dados 'Bilhete' pode ser um Trem, Ônibus ou Avião, com campos específicos para cada tipo.
data Bilhete
  = Trem { de_trem :: String, para_trem :: String, classe_trem :: ClasseTrem }
  | Onibus { de_onibus :: String, para_onibus :: String }
  | Aviao { de_aviao :: String, para_aviao :: String, classe_aviao :: ClasseAviao }
  deriving (Show, Eq)

-- Uma 'Viagem' é definida como uma lista de 'Bilhetes'.
type Viagem = [Bilhete]

-- Funções para extrair as cidades de partida e destino de qualquer tipo de bilhete.
partida :: Bilhete -> String
partida (Trem de _ _) = de
partida (Onibus de _) = de
partida (Aviao de _ _) = de

destino :: Bilhete -> String
destino (Trem _ para _) = para
destino (Onibus _ para) = para
destino (Aviao _ para _) = para

-- Verifica se a sequência de cidades em uma viagem é válida (se o destino de um bilhete é a partida do próximo).
validaViagem :: Viagem -> Bool
validaViagem [] = True -- Viagem vazia é válida.
validaViagem [_] = True -- Viagem com um único bilhete é válida.
validaViagem viagem =
  let pares = zip viagem (tail viagem) -- Cria pares de bilhetes adjacentes (bilhete atual e próximo).
  in all (\(b1, b2) -> destino b1 == partida b2) pares -- 'all' verifica se o predicado é verdadeiro para todos os pares.
  
-- Q2. Tipos e funções para Mensagens
-- Define tipos de dados para o identificador (Contato ou Fone), fonte da mensagem e data/horário.
data Identificador = Contato String | Fone String deriving (Show, Eq, Ord)
data Fonte = WhatsApp | LinkedIn | Facebook deriving (Show, Eq, Ord)
data Data = Data { ano :: Int, mes :: Int, dia :: Int } deriving (Show, Eq, Ord)
data Horario = Horario { hora :: Int, min :: Int } deriving (Show, Eq, Ord)

-- O tipo 'Mensagem' é um registro com todos os campos relevantes.
data Mensagem = Mensagem
  { dataMsg    :: Data
  , horarioMsg :: Horario
  , remetente  :: Identificador
  , texto      :: String
  , fonte      :: Fonte
  } deriving (Show, Eq, Ord)

-- Lista de exemplo de mensagens.
mensagensExemplo :: [Mensagem]
mensagensExemplo =
  [ Mensagem (Data 2015 8 13) (Horario 10 30) (Contato "Augusto Costa") "A apresentação de 13h foi cancelada" WhatsApp
  , Mensagem (Data 2015 8 11) (Horario 8 50) (Fone "3232-3232") "Reunião 14h - Lições para Trabalho em Equipe" WhatsApp
  , Mensagem (Data 2015 8 15) (Horario 6 57) (Contato "Ana Paula Silva") "Banco de Talentos da USP" LinkedIn
  , Mensagem (Data 2015 8 14) (Horario 11 10) (Contato "Flávio Rocha") "Veja o link aaaaa.bbb.com" Facebook
  ]
  
-- Q2. b) Ordenação
-- 'ordenarMetodo1' usa a função 'sort' que se baseia na instância de 'Ord' derivada automaticamente, que ordena os campos na ordem em que são declarados.
ordenarMetodo1 :: [Mensagem] -> [Mensagem]
ordenarMetodo1 = sort

-- 'ordenarMetodo2' usa 'sortBy' com uma função de comparação explícita para garantir que a ordenação seja feita primeiro pela data e depois pelo horário.
ordenarMetodo2 :: [Mensagem] -> [Mensagem]
ordenarMetodo2 = sortBy (comparing (\m -> (dataMsg m, horarioMsg m)))

-- Q2. c) Últimas duas mensagens de um contato
-- Filtra as mensagens de um contato e retorna as duas mais recentes.
ultimasDuasMensagens :: String -> [Mensagem] -> [Mensagem]
ultimasDuasMensagens nomeProcurado msgs =
  let mensagensDoContato = filter (ehDoContato nomeProcurado) msgs -- Filtra a lista de mensagens para obter apenas as do contato desejado.
  in take 2 (reverse mensagensDoContato) -- Inverte a lista (para ter as mais recentes no início) e pega as duas primeiras.

-- Função auxiliar que verifica se uma mensagem é de um contato com um nome específico.
ehDoContato :: String -> Mensagem -> Bool
ehDoContato nomeDesejado msg = case remetente msg of
  Contato nome -> nome == nomeDesejado -- Se o remetente for um Contato, compara o nome.
  _            -> False -- Caso contrário, não é o contato desejado.
  
-- Q3. Funções para Árvore Binária
-- Define o tipo de dados para uma árvore binária.
data ArvBin a = Vazia | No (ArvBin a) a (ArvBin a) deriving (Show)

-- 'mapA' aplica uma função a cada valor na árvore.
mapA :: (a -> b) -> ArvBin a -> ArvBin b
mapA _ Vazia = Vazia -- Caso base: árvore vazia.
mapA f (No esq val dir) = No (mapA f esq) (f val) (mapA f dir) -- Mapeia a função para o valor do nó e chama 'mapA' recursivamente para os sub-nós esquerdo e direito.

-- 'filterA' mantém os nós que satisfazem o predicado, podando os ramos que não o fazem.
filterA :: (a -> Bool) -> ArvBin a -> ArvBin a
filterA _ Vazia = Vazia -- Caso base: árvore vazia.
filterA p (No esq val dir)
  | p val     = No (filterA p esq) val (filterA p dir) -- Se o valor do nó satisfaz o predicado, mantém o nó e filtra os sub-nós.
  | otherwise = Vazia -- Caso contrário, o nó e todos os seus sub-nós são descartados.

-- 'elemA' verifica se um elemento existe na árvore.
elemA :: Eq a => a -> ArvBin a -> Bool
elemA _ Vazia = False -- Caso base: elemento não encontrado em árvore vazia.
elemA x (No esq val dir) = (x == val) || elemA x esq || elemA x dir -- Verifica se o valor do nó é o procurado ou se ele existe na sub-árvore esquerda ou na direita.

-- Q4. Funções para Móbile
-- Define o tipo de dados para um móbile.
data Mobile = Pendente Int | Barra Mobile Mobile deriving (Show)

-- 'peso' calcula o peso total de um móbile.
peso :: Mobile -> Int
peso (Pendente p)   = p -- O peso de um pendente é seu próprio valor.
peso (Barra m1 m2)  = peso m1 + peso m2 -- O peso de uma barra é a soma dos pesos dos móbiles pendurados nela.

-- 'balanceado' verifica se o móbile está perfeitamente balanceado.
balanceado :: Mobile -> Bool
balanceado (Pendente _)  = True -- Um único pendente é sempre balanceado.
balanceado (Barra m1 m2) =
  (peso m1 == peso m2) && balanceado m1 && balanceado m2 -- Uma barra está balanceada se os pesos de seus sub-móbiles são iguais E se cada sub-móbile também está balanceado.
  
-- Q5. Funções para Rodada de Jogos
-- Define tipos de dados para representar jogos de futebol.
type Gols = Int
type Time = String
type Jogo = ((Time, Gols), (Time, Gols))
type Rodada = [Jogo]

-- 'diferentes' verifica se nenhum time joga contra si mesmo.
diferentes :: Rodada -> Bool
diferentes r = all (\((t1, _), (t2, _)) -> t1 /= t2) r -- 'all' verifica se para todos os jogos da rodada, os times são diferentes.

-- 'semrepet' verifica se nenhum time joga mais de uma vez na rodada.
semrepet :: Rodada -> Bool
semrepet r =
  let timesNaRodada = concatMap (\((t1, _), (t2, _)) -> [t1, t2]) r -- Cria uma lista com todos os times de todos os jogos.
      timesUnicos   = nub timesNaRodada -- 'nub' remove os times duplicados.
  in length timesNaRodada == length timesUnicos -- Se a lista original tiver o mesmo tamanho da lista sem duplicatas, significa que não há times repetidos.

-- 'times' retorna uma lista de todos os times que participam da rodada, sem duplicatas.
times :: Rodada -> [Time]
times r = nub (concatMap (\((t1, _), (t2, _)) -> [t1, t2]) r) -- Coleta todos os times e remove as duplicatas com 'nub'.

-- 'empates' retorna uma lista dos pares de times que empataram.
empates :: Rodada -> [(Time, Time)]
empates r =
  let jogosEmpatados = filter (\((_, g1), (_, g2)) -> g1 == g2) r -- Filtra os jogos onde os gols de ambos os times são iguais.
  in map (\((t1, _), (t2, _)) -> (t1, t2)) jogosEmpatados -- Mapeia a lista de jogos para uma lista de tuplas de times.

-- 'pontos' calcula a pontuação de cada time na rodada.
pontos :: Rodada -> [(Time, Int)]
pontos r =
  let timesUnicos = times r
  in map (\time -> (time, calculaPontosPara time r)) timesUnicos -- Para cada time único, chama a função 'calculaPontosPara' para determinar a pontuação.

-- Função auxiliar para 'pontos' que calcula a pontuação de um time específico em uma rodada.
calculaPontosPara :: Time -> Rodada -> Int
calculaPontosPara time rodada = foldl (\acc ((t1, g1), (t2, g2)) ->
  if time == t1 then
    if g1 > g2 then acc + 3
    else if g1 == g2 then acc + 1
    else acc
  else if time == t2 then
    if g2 > g1 then acc + 3
    else if g1 == g2 then acc + 1
    else acc
  else acc
  ) 0 rodada -- Usa 'foldl' para somar os pontos de cada jogo, com base nos resultados.

-- Q6. 'buscaSegura'
-- Busca o índice de um elemento em uma lista, retornando o resultado dentro de um 'Maybe' para lidar com a possibilidade do elemento não existir.
buscaSegura :: Eq a => a -> [a] -> Maybe Int
buscaSegura x lista =
  let listaComIndices = zip [0..] lista -- Cria uma lista de pares (índice, elemento).
      maybePar = find (\(indice, elemento) -> elemento == x) listaComIndices -- 'find' procura o primeiro par que tem o elemento desejado.
  in fmap fst maybePar -- 'fmap' aplica a função 'fst' (que pega o primeiro elemento da tupla, o índice) dentro do 'Maybe'.
  
-- Q7. 'validarIdade'
-- Usa o tipo 'Either' para validar a idade de uma pessoa, retornando um 'Left' com uma mensagem de erro ou um 'Right' com os dados da pessoa se a idade for válida.
data DadosPessoa = DadosPessoa
  { nome :: String
  , idade :: Int
  } deriving (Show, Eq)

validarIdade :: DadosPessoa -> Either String DadosPessoa
validarIdade pessoa
  | idadePessoa < 0 || idadePessoa >= 120 = Left "Idade inválida" -- Se a idade for inválida, retorna 'Left'.
  | otherwise                             = Right pessoa -- Se a idade for válida, retorna 'Right'.
  where idadePessoa = idade pessoa
  
-- Q8. 'mediaAprovação'
-- Usa o tipo 'Maybe' para calcular a média de um estudante, retornando 'Nothing' se a lista de notas estiver vazia para evitar erro de divisão por zero.
data Estudante = Estudante
  { matricula :: Int
  , nomeEst   :: String
  , notas     :: [Double]
  } deriving (Show)

mediaAprovação :: Estudante -> Maybe Double
mediaAprovação estudante
  | null listaDeNotas = Nothing -- Se a lista de notas for vazia, retorna 'Nothing'.
  | otherwise         = Just (sum listaDeNotas / fromIntegral (length listaDeNotas)) -- Calcula a média e a coloca dentro de um 'Just'.
  where listaDeNotas = notas estudante
  
-- Q9. 'autenticar'
-- Autentica um usuário em um banco de dados, retornando 'Right' com o usuário se as credenciais estiverem corretas, ou 'Left' com uma mensagem de erro.
data Usuario = Usuario
  { login :: String
  , senha :: String
  } deriving (Show, Eq)

autenticar :: String -> String -> [Usuario] -> Either String Usuario
autenticar loginInput senhaInput db =
  case find (credenciaisCorretas loginInput senhaInput) db of -- Usa 'find' para procurar um usuário que corresponda às credenciais fornecidas.
    Just usuario -> Right usuario -- Se um usuário for encontrado, retorna 'Right' com o usuário.
    Nothing      -> Left "Credenciais inválidas" -- Se nenhum usuário for encontrado, retorna 'Left'.

-- Função auxiliar que verifica se o login e a senha correspondem aos do usuário.
credenciaisCorretas :: String -> String -> Usuario -> Bool
credenciaisCorretas loginInput senhaInput usuario =
  login usuario == loginInput && senha usuario == senhaInput
  
-- Q10. 'emprestarLivro'
-- Usa o tipo 'Maybe' para representar o empréstimo de um livro. Se o livro já estiver emprestado, retorna 'Nothing'.
data Livro = Livro
  { titulo     :: String
  , autor      :: String
  , emprestado :: Bool
  } deriving (Show, Eq)

emprestarLivro :: Livro -> Maybe Livro
emprestarLivro livro
  | emprestado livro = Nothing -- Se 'emprestado' for True, retorna 'Nothing'.
  | otherwise        = Just (livro { emprestado = True }) -- Caso contrário, cria uma nova cópia do registro com 'emprestado' como True e a envolve em 'Just'.
