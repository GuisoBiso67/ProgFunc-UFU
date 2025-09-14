import Data.List
import Data.Ord

--Q1
segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos p = filter (p . head) . groupBy (\x y -> p x == p y) 

segmentos2 :: (a -> Bool) -> [a] -> [[a]]
segmentos2 p [] = [] -- Base case: an empty list has no segments.
segmentos2 p (x:xs)
  -- If the first element satisfies the predicate...
  | p x       = let (segment, rest) = span p (x:xs)
                in segment : segmentos2 p rest
  -- If the first element does not satisfy the predicate...
  | otherwise = segmentos2 p xs -- ...skip it and check the rest of the list.
  
-- Q1) a)
linhas :: String -> [String]
linhas xs = segmentos (/= '\n') xs
-- Q1) b)
palavras :: String -> [String]
palavras xs = segmentos (not . isSpace) xs
isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r']

-- Q2)a)
foldn f e 0 = e
foldn f e n = f (foldn f e (n-1))

replica :: Int -> a -> [a]
replica n v = foldn (v:) [] n
-- b)
dupS :: String -> String
dupS str = concat (map (\c -> replica 2 c) str)

-- Q3
prefixo :: String -> [String] -> [String]
prefixo pre telefs = filter (concorda pre) telefs
  where concorda :: String -> String -> Bool
        concorda [] _ = True
        concorda (x:xs) (y:ys) = (x==y) && (concorda xs ys)
        concorda (x:xs) [] = False
        
concorda :: String -> String -> Bool
concorda [] _ = True
concorda (x:xs) (y:ys) = (x==y) && (concorda xs ys)
concorda (x:xs) [] = False

prefixoR :: String -> [String] -> [String]
prefixoR [] _ = []
prefixoR _ [] = []
prefixoR pre tele
  | concorda pre (head tele) = (head tele) : prefixoR pre (tail tele)
  | otherwise = prefixoR pre (tail tele)

-- Q4
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
-- Q5)a)
pertence :: Eq a => a -> [a] -> Bool
pertence v = foldr (\x acc -> (x == v) || acc) False
-- Q5)b)
remdups :: Eq a => [a] -> [a]
remdups = foldr (\x acc -> if null acc || x /= head acc then x : acc else acc) []
-- Q6)a)
todos :: (a -> Bool) -> [a] -> Bool
todos p = foldr (\x acc -> p x && acc) True

positivos :: [Int] -> Bool
positivos = todos (>0)
-- Q6)b)
algum :: (a -> Bool) -> [a] -> Bool
algum p = foldr (\x acc -> p x || acc) False

membro :: Eq a => a -> [a] -> Bool
membro v = algum (== v)
-- Q7
separa :: [a] -> ([a], [a])
separa = foldr (\x (l, r) -> (x:r, l)) ([], [])

---------------------------- PARTE 2 ------------------------------
---------------------------- PARTE 2 ------------------------------
---------------------------- PARTE 2 ------------------------------

--Q1
-- Tipos auxiliares para as classes de viagem
data ClasseTrem = Primeira | Segunda deriving (Show, Eq)
data ClasseAviao = Executiva | Economica deriving (Show, Eq)

-- Tipo principal para representar qualquer bilhete
data Bilhete
  = Trem { de_trem :: String, para_trem :: String, classe_trem :: ClasseTrem }
  | Onibus { de_onibus :: String, para_onibus :: String }
  | Aviao { de_aviao :: String, para_aviao :: String, classe_aviao :: ClasseAviao }
  deriving (Show, Eq)

-- Uma Viagem é simplesmente uma lista de Bilhetes
type Viagem = [Bilhete]

-- Função auxiliar para extrair a cidade de partida de qualquer bilhete
partida :: Bilhete -> String
partida (Trem de _ _) = de
partida (Onibus de _) = de
partida (Aviao de _ _) = de

-- Função auxiliar para extrair a cidade de destino de qualquer bilhete
destino :: Bilhete -> String
destino (Trem _ para _) = para
destino (Onibus _ para) = para
destino (Aviao _ para _) = para

-- Verifica se a sequência de cidades em uma viagem é válida
validaViagem :: Viagem -> Bool
-- Viagens vazias ou com um único bilhete são sempre válidas
validaViagem [] = True
validaViagem [_] = True
-- Para viagens com 2+ bilhetes, compara os bilhetes adjacentes
validaViagem viagem =
  -- 'zip' cria pares de elementos adjacentes. Ex: [1,2,3,4] -> [(1,2), (2,3), (3,4)]
  let pares = zip viagem (tail viagem)
  -- 'all' verifica se a condição é verdadeira para todos os pares
  in all (\(b1, b2) -> destino b1 == partida b2) pares
  
--Q2 
--Q2A
-- Tipos para identificar o remetente e a fonte da mensagem
data Identificador = Contato String | Fone String deriving (Show, Eq, Ord)
data Fonte = WhatsApp | LinkedIn | Facebook deriving (Show, Eq, Ord)

-- Tipos para data e horário para facilitar a ordenação
data Data = Data { ano :: Int, mes :: Int, dia :: Int } deriving (Show, Eq, Ord)
data Horario = Horario { hora :: Int, min :: Int } deriving (Show, Eq, Ord)

-- O registro principal que representa uma mensagem
data Mensagem = Mensagem
  { dataMsg    :: Data
  , horarioMsg :: Horario
  , remetente  :: Identificador
  , texto      :: String
  , fonte      :: Fonte
  } deriving (Show, Eq, Ord)

-- Exemplo de uma lista de mensagens
mensagensExemplo :: [Mensagem]
mensagensExemplo =
  [ Mensagem (Data 2015 8 13) (Horario 10 30) (Contato "Augusto Costa") "A apresentação de 13h foi cancelada" WhatsApp
  , Mensagem (Data 2015 8 11) (Horario 8 50) (Fone "3232-3232") "Reunião 14h - Lições para Trabalho em Equipe" WhatsApp
  , Mensagem (Data 2015 8 15) (Horario 6 57) (Contato "Ana Paula Silva") "Banco de Talentos da USP" LinkedIn
  , Mensagem (Data 2015 8 14) (Horario 11 10) (Contato "Flávio Rocha") "Veja o link aaaaa.bbb.com" Facebook
  ]
  
--Q2B
-- Método 1: Usando 'sort', que utiliza a instância de 'Ord' derivada automaticamente
ordenarMetodo1 :: [Mensagem] -> [Mensagem]
ordenarMetodo1 = sort

-- Método 2: Usando 'sortBy' com uma função de comparação explícita
-- Isso nos dá mais controle e não depende da ordem dos campos no registro.
ordenarMetodo2 :: [Mensagem] -> [Mensagem]
ordenarMetodo2 = sortBy (comparing (\m -> (dataMsg m, horarioMsg m)))

--Q2C
-- Dado um nome e uma lista de mensagens (já ordenada), retorna as 2 mais recentes
ultimasDuasMensagens :: String -> [Mensagem] -> [Mensagem]
ultimasDuasMensagens nomeProcurado msgs =
  -- 1. Filtra a lista para pegar apenas mensagens do contato desejado
  let mensagensDoContato = filter (ehDoContato nomeProcurado) msgs
  -- 2. Pega as duas últimas mensagens da lista filtrada.
  --    Como a lista está ordenada da mais antiga para a mais nova,
  --    pegamos as duas últimas.
  in take 2 (reverse mensagensDoContato)

-- Função auxiliar que verifica se uma mensagem pertence a um contato com um certo nome
ehDoContato :: String -> Mensagem -> Bool
ehDoContato nomeDesejado msg = case remetente msg of
  Contato nome -> nome == nomeDesejado
  _            -> False
  
--Q3
data ArvBin a = Vazia | No (ArvBin a) a (ArvBin a) deriving (Show)

-- mapA: aplica uma função a cada valor na árvore
mapA :: (a -> b) -> ArvBin a -> ArvBin b
mapA _ Vazia = Vazia
mapA f (No esq val dir) = No (mapA f esq) (f val) (mapA f dir)

-- filterA: mantém apenas os nós que satisfazem o predicado,
-- podando os ramos cujos pais não satisfazem.
filterA :: (a -> Bool) -> ArvBin a -> ArvBin a
filterA _ Vazia = Vazia
filterA p (No esq val dir)
  | p val     = No (filterA p esq) val (filterA p dir)
  | otherwise = Vazia

-- elemA: verifica se um elemento existe na árvore
elemA :: Eq a => a -> ArvBin a -> Bool
elemA _ Vazia = False
elemA x (No esq val dir) = (x == val) || elemA x esq || elemA x dir

--Q4
data Mobile = Pendente Int | Barra Mobile Mobile deriving (Show)

-- a) peso: calcula o peso total de um móbile
peso :: Mobile -> Int
peso (Pendente p)   = p
peso (Barra m1 m2)  = peso m1 + peso m2

-- b) balanceado: verifica se o móbile está perfeitamente balanceado
balanceado :: Mobile -> Bool
balanceado (Pendente _)  = True -- Um único pendente é sempre balanceado
balanceado (Barra m1 m2) =
  (peso m1 == peso m2) && balanceado m1 && balanceado m2
  
--Q5
type Gols = Int
type Time = String
type Jogo = ((Time, Gols), (Time, Gols))
type Rodada = [Jogo]

-- Exemplo de rodada
rodadaExemplo :: Rodada
rodadaExemplo =
  [ (("Atletico-MG", 2), ("Cruzeiro", 2))
  , (("Flamengo", 3), ("Vasco", 1))
  , (("Corinthians", 0), ("Palmeiras", 1))
  ]
-- a) diferentes: verifica se nenhum time joga com ele próprio
diferentes :: Rodada -> Bool
diferentes r = all (\((t1, _), (t2, _)) -> t1 /= t2) r

-- b) semrepet: verifica se nenhum time joga mais de uma vez na rodada
semrepet :: Rodada -> Bool
semrepet r =
  let timesNaRodada = concatMap (\((t1, _), (t2, _)) -> [t1, t2]) r
      timesUnicos   = nub timesNaRodada -- 'nub' remove duplicatas
  in length timesNaRodada == length timesUnicos

-- c) times: retorna uma lista com os times que participam da rodada
times :: Rodada -> [Time]
times r = nub (concatMap (\((t1, _), (t2, _)) -> [t1, t2]) r)

-- d) empates: retorna uma lista dos pares de times que empataram
empates :: Rodada -> [(Time, Time)]
empates r =
  let jogosEmpatados = filter (\((_, g1), (_, g2)) -> g1 == g2) r
  in map (\((t1, _), (t2, _)) -> (t1, t2)) jogosEmpatados

-- e) pontos: calcula os pontos de cada time na rodada
pontos :: Rodada -> [(Time, Int)]
pontos r =
  let timesUnicos = times r
  in map (\time -> (time, calculaPontosPara time r)) timesUnicos

-- Função auxiliar para 'pontos'
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
  ) 0 rodada
  
--Q6
-- Uma forma é usar 'find' com 'zip' para obter o elemento e seu índice
buscaSegura :: Eq a => a -> [a] -> Maybe Int
buscaSegura x lista =
  let listaComIndices = zip [0..] lista
      maybePar = find (\(indice, elemento) -> elemento == x) listaComIndices
  in fmap fst maybePar -- fmap aplica a função 'fst' dentro do Maybe
  
--Q7
data DadosPessoa = DadosPessoa
  { nome :: String
  , idade :: Int
  } deriving (Show, Eq)

validarIdade :: DadosPessoa -> Either String DadosPessoa
validarIdade pessoa
  | idadePessoa < 0 || idadePessoa >= 120 = Left "Idade inválida"
  | otherwise                             = Right pessoa
  where idadePessoa = idade pessoa
  
--Q8
data Estudante = Estudante
  { matricula :: Int
  , nomeEst   :: String
  , notas     :: [Double]
  } deriving (Show)

mediaAprovação :: Estudante -> Maybe Double
mediaAprovação estudante
  | null listaDeNotas = Nothing
  | otherwise         = Just (sum listaDeNotas / fromIntegral (length listaDeNotas))
  where listaDeNotas = notas estudante
  
--Q9
data Usuario = Usuario
  { login :: String
  , senha :: String
  } deriving (Show, Eq)

autenticar :: String -> String -> [Usuario] -> Either String Usuario
autenticar loginInput senhaInput db =
  case find (credenciaisCorretas loginInput senhaInput) db of
    Just usuario -> Right usuario
    Nothing      -> Left "Credenciais inválidas"

-- Função auxiliar para 'find'
credenciaisCorretas :: String -> String -> Usuario -> Bool
credenciaisCorretas loginInput senhaInput usuario =
  login usuario == loginInput && senha usuario == senhaInput
  
--Q10
data Livro = Livro
  { titulo     :: String
  , autor      :: String
  , emprestado :: Bool
  } deriving (Show, Eq)

emprestarLivro :: Livro -> Maybe Livro
emprestarLivro livro
  | emprestado livro = Nothing
  | otherwise        = Just (livro { emprestado = True })
