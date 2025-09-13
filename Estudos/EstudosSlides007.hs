-- TIPOS ALGÉBRICOS --

-- Tipos Sinônimos
{-
type Ponto = (Double, Double)
type ListaPares a b = [(a,b)]
distanciaOrigem :: Ponto -> Double
distanciaOrigem (x,y) = sqrt(x^2 + y^2)
-}

type Par a = (a,a)
somaPares :: Par Int -> Int
somaPares (x,y) = x+y

type Matriz a = [[a]]
matrizIdentidade :: Matriz Int
matrizIdentidade = [[1,0],[0,1]]

type Nome = String
type Idade = Int
infoPessoa :: Nome -> Idade -> String
infoPessoa nome idade = nome ++ " tem " ++ show idade ++ " anos."

-- não significa criar novos tipos e não pode ser recursivos

-- Tipos Algébricos
data Mes = Jan | Fev | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
-- algo tipo o "Bool" => data Bool = True | False

data Cor = Azul | Amarelo | Verde | Vermelho
fria :: Cor -> Bool
fira Azul = True
fria Verde = True
fria _ = False
quente :: Cor -> Bool
quente Amarelo = True
quente Vermelho = True
quente _ = False

data CoordCart = Coord Double Double
somaVet :: CoordCart -> CoordCart -> CoordCart
somaVet (Coord x1 y1) (Coord x2 y2) = Coord (x1+x2) (y1+y2)

data Figura = Retangulo Double Double
            | Circulo Double
            | Triangulo Double Double
area :: Figura -> Double
area (Retangulo b h) = b*h
area (Circulo r) = pi*r^2
area (Triangulo b h) = (b*h) / 2

-- as definições de tipos algébricos podem ser recursivas:
{-
data Natural = Zero | Suc Natural
deNatural2Int :: Natural -> Int
deNatural2Int Zero = 0
deNatural2Int (Suc n) = 1 + deNatural2Int n
-}

data Lista a = Vazia
             | Cons a (Lista a)

comprimento :: Lista a -> Int
comprimento Vazia       = 0
comprimento (Cons _ xs) = 1 + comprimento xs

-- Tipo Produto
data Ponto = Ponto Double Double -- [1] = [2] [3]
-- [1] = construtor de tipo; [2] = construtor de dados; [3] = tipos envolvidos
   deriving (Show, Eq) -- agora dá para imprimir e comparar
-- Ponto tem |Double| x |Double| possibilidades
   
-- Tipo Soma
data Ponto2 = Ponto2D Double Double | Ponto3D Double Double Double
-- Ponto2 tem |Double| x |Double| + |Double| x |Double| x |Double| possibilidades

-- Tipos Algébricos Recursivos
data Expr = Lit Int |
            Soma Expr Expr |
            Sub Expr Expr
avalia :: Expr -> Int
avalia (Lit n) = n
avalia (Soma e1 e2) = (avalia e1) + (avalia e2)
avalia (Sub e1 e2) = (avalia e1) - (avalia e2)

-- Tipos Comuns

-- maybe é uma alternativa segura para null
headSegura :: [a] -> Maybe a
headSegura [] = Nothing
headSegura (x:_) = Just x

-- either armazena uma informação se algo der errado, tipo uma mensagem de erro
headSegura2 :: [a] -> Either String a
headSegura2 [] = Left "Sem cabeca!"
headSegura2 (x:_) = Right x

-- Registros (tipo o struct)
data Pessoa = Pessoa 
   {
   idade :: Int,
   nome :: String

   }
   deriving (Show, Eq)
-- p = Pessoa { idade = 25, nome = "Joao" }
-- p1 = Pessoa 25 "Maria"
-- p1Update = p1 {idade = 24}

-- Implementando Show ao Expr:
instance Show Expr where
   show (Lit n) = show n
   show (Soma e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
   show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
   
-- Instanciando Natural

