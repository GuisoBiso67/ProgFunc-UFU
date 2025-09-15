import System.IO
import Data.List
import Data.Char
import Data.Ord

-- Q1
escreverLerArquivo :: IO ()
escreverLerArquivo = do
    -- 1. Cria ou abre o arquivo para escrita
    putStrLn "Digite os caracteres (digite '0' para parar):"
    
    handle <- openFile "arq.txt" WriteMode
    escreverCaracteres handle
    hClose handle
    
    putStrLn "\nConteúdo do arquivo:"
    
    -- 2. Abre o arquivo para leitura
    handle <- openFile "arq.txt" ReadMode
    lerCaracteres handle
    hClose handle
    where
      escreverCaracteres handle = do
        c <- getChar
        if c == '0'
          then return ()
          else do
            hPutChar handle c
            escreverCaracteres handle
            
      lerCaracteres handle = do
        eof <- hIsEOF handle
        if eof
          then return ()
          else do
            c <- hGetChar handle
            putChar c
            lerCaracteres handle
            
-- Q2        
contarLinhas :: IO ()
contarLinhas = do
    putStrLn "Digite o nome do arquivo:"
    nomeArquivo <- getLine
    
    conteudo <- readFile nomeArquivo
    let linhas = lines conteudo
    let numLinhas = length linhas
    
    putStrLn ("O arquivo tem " ++ show numLinhas ++ " linhas.")

-- Q3
contarVogais :: IO ()
contarVogais = do
    putStrLn "Digite o nome do arquivo:"
    nomeArquivo <- getLine
    
    conteudo <- readFile nomeArquivo
    let vogais = "aeiouAEIOU"
    let numVogais = length $ filter (`elem` vogais) conteudo
    
    putStrLn ("O arquivo tem " ++ show numVogais ++ " vogais.")

-- Q4
contarCaracteres :: IO ()
contarCaracteres = do
    putStrLn "Digite o nome do arquivo:"
    nomeArquivo <- getLine
    putStrLn "Digite o caractere a ser contado:"
    charContar <- getChar
    
    conteudo <- readFile nomeArquivo
    let numOcorrencias = length $ filter (== charContar) conteudo
    
    putStrLn ("O caractere '" ++ [charContar] ++ "' ocorre " ++ show numOcorrencias ++ " vezes no arquivo.")

-- Q5
juntarArquivos :: IO ()
juntarArquivos = do
    putStrLn "Digite o nome do primeiro arquivo:"
    arq1 <- getLine
    putStrLn "Digite o nome do segundo arquivo:"
    arq2 <- getLine
    putStrLn "Digite o nome do arquivo de saída:"
    arqSaida <- getLine
    
    conteudo1 <- readFile arq1
    conteudo2 <- readFile arq2
    
    let conteudoFinal = conteudo1 ++ conteudo2
    
    writeFile arqSaida conteudoFinal
    
    putStrLn "Arquivos unidos com sucesso!"

-- Q6
acharCidadeMaisPopulosa :: IO ()
acharCidadeMaisPopulosa = do
    putStrLn "Digite o nome do arquivo de entrada:"
    arqEntrada <- getLine
    putStrLn "Digite o nome do arquivo de saída:"
    arqSaida <- getLine
    
    conteudo <- readFile arqEntrada
    let linhas = lines conteudo
    let cidades = map parseLinha linhas
    let cidadeMaisPopulosa = maximumBy (comparing snd) cidades
    
    writeFile arqSaida (fst cidadeMaisPopulosa ++ " " ++ show (snd cidadeMaisPopulosa))
    
    putStrLn "Arquivo de saída gerado com a cidade mais populosa."

-- Helper para converter "NomeDaCidade 123456" em ("NomeDaCidade", 123456)
parseLinha :: String -> (String, Int)
parseLinha linha =
    let partes = words linha
        nome = unwords (init partes)
        populacao = read (last partes) :: Int
    in (nome, populacao)
 
-- Q7
cadastrarContatos :: IO ()
cadastrarContatos = do
    putStrLn "Digite o nome do arquivo para cadastro:"
    nomeArquivo <- getLine
    
    putStrLn "Digite o nome e o telefone (telefone '0' para parar):"
    handle <- openFile nomeArquivo WriteMode
    escreverContatos handle
    hClose handle
    
    putStrLn "Cadastro finalizado!"
    where
      escreverContatos handle = do
        putStr "Nome: "
        nome <- getLine
        putStr "Telefone: "
        telefone <- getLine
        
        if telefone == "0"
          then return ()
          else do
            hPutStrLn handle (nome ++ "," ++ telefone)
            escreverContatos handle
            
-- Q8
substituirVogais :: IO ()
substituirVogais = do
    putStrLn "Digite o nome do arquivo de entrada:"
    arqEntrada <- getLine
    putStrLn "Digite o nome do arquivo de saída:"
    arqSaida <- getLine
    
    conteudo <- readFile arqEntrada
    let novoConteudo = map substituirVogal conteudo
    
    writeFile arqSaida novoConteudo
    
    putStrLn "Vogais substituídas com sucesso!"
    where
      substituirVogal c
        | toLower c `elem` "aeiou" = '*'
        | otherwise = c
        
-- Q9 
converterParaMaiusculas :: IO ()
converterParaMaiusculas = do
    putStrLn "Digite o nome do arquivo de entrada:"
    arqEntrada <- getLine
    putStrLn "Digite o nome do arquivo de saída:"
    arqSaida <- getLine
    
    conteudo <- readFile arqEntrada
    let conteudoMaiusculo = map toUpper conteudo
    
    writeFile arqSaida conteudoMaiusculo
    
    putStrLn "Arquivo convertido para maiúsculas com sucesso!"

-- Q10
contarPalavras :: IO ()
contarPalavras = do
    putStrLn "Digite o nome do arquivo:"
    nomeArquivo <- getLine
    putStrLn "Digite a palavra a ser contada:"
    palavra <- getLine
    
    conteudo <- readFile nomeArquivo
    let palavrasNoArquivo = map (map toLower . filter isLetter) (words conteudo)
    let palavraProcurada = map toLower palavra
    
    let numOcorrencias = length $ filter (== palavraProcurada) palavrasNoArquivo
    
    putStrLn ("A palavra '" ++ palavra ++ "' ocorre " ++ show numOcorrencias ++ " vezes no arquivo.")

-- Q11
-- Função que escreve uma lista de inteiros em um arquivo
escreverInteiros :: String -> [Int] -> IO ()
escreverInteiros nomeArquivo lista = do
    let conteudo = intercalate "\n" (map show lista)
    writeFile nomeArquivo conteudo

-- Função que lê um arquivo e devolve uma lista de inteiros
lerInteiros :: String -> IO [Int]
lerInteiros nomeArquivo = do
    conteudo <- readFile nomeArquivo
    let linhas = lines conteudo
    let listaDeInteiros = map read linhas :: [Int]
    return listaDeInteiros

-- Exemplo de uso
exemplo11 :: IO ()
exemplo11 = do
    let nomeArq = "lista_inteiros.txt"
    let lista = [10, 20, 30, 40, 50]
    
    putStrLn "Escrevendo a lista [10, 20, 30, 40, 50] no arquivo 'lista_inteiros.txt'..."
    escreverInteiros nomeArq lista
    
    putStrLn "Lendo a lista do arquivo 'lista_inteiros.txt'..."
    listaLida <- lerInteiros nomeArq
    putStrLn ("Lista lida: " ++ show listaLida)
