import System.IO
import Data.List
import Data.Char
import Data.Ord

-- Q1. 'escreverLerArquivo'
-- Permite que o usuário grave caracteres em um arquivo e depois leia e mostre seu conteúdo.
escreverLerArquivo :: IO ()
escreverLerArquivo = do
    putStrLn "Digite os caracteres (digite '0' para parar):" -- Informa o usuário sobre a entrada.
    
    handle <- openFile "arq.txt" WriteMode -- Abre o arquivo 'arq.txt' em modo de escrita.
    escreverCaracteres handle -- Chama a função auxiliar para escrever.
    hClose handle -- Fecha o arquivo.
    
    putStrLn "\nConteúdo do arquivo:" -- Prepara para a leitura.
    
    handle <- openFile "arq.txt" ReadMode -- Reabre o arquivo em modo de leitura.
    lerCaracteres handle -- Chama a função auxiliar para ler.
    hClose handle -- Fecha o arquivo.
    where
      escreverCaracteres handle = do
        c <- getChar -- Lê um caractere do teclado.
        if c == '0' -- Se o caractere for '0', a recursão para.
          then return ()
          else do
            hPutChar handle c -- Escreve o caractere no arquivo.
            escreverCaracteres handle -- Chama recursivamente.
            
      lerCaracteres handle = do
        eof <- hIsEOF handle -- Verifica se chegou ao fim do arquivo.
        if eof -- Se for o fim, a recursão para.
          then return ()
          else do
            c <- hGetChar handle -- Lê um caractere do arquivo.
            putChar c -- Imprime o caractere na tela.
            lerCaracteres handle -- Chama recursivamente.
            
-- Q2. 'contarLinhas'
-- Recebe o nome de um arquivo e exibe o número de linhas que ele contém.        
contarLinhas :: IO ()
contarLinhas = do
    putStrLn "Digite o nome do arquivo:" -- Pede o nome do arquivo ao usuário.
    nomeArquivo <- getLine -- Lê a entrada do usuário.
    
    conteudo <- readFile nomeArquivo -- Lê o conteúdo completo do arquivo em uma única string.
    let linhas = lines conteudo -- Usa a função 'lines' para dividir a string em uma lista de linhas.
    let numLinhas = length linhas -- Obtém o número de linhas contando os elementos da lista.
    
    putStrLn ("O arquivo tem " ++ show numLinhas ++ " linhas.") -- Exibe o resultado.

-- Q3. 'contarVogais'
-- Conta o número de vogais em um arquivo de texto.
contarVogais :: IO ()
contarVogais = do
    putStrLn "Digite o nome do arquivo:" -- Pede o nome do arquivo.
    nomeArquivo <- getLine -- Lê o nome.
    
    conteudo <- readFile nomeArquivo -- Lê o conteúdo.
    let vogais = "aeiouAEIOU" -- Define a string de vogais.
    let numVogais = length $ filter (`elem` vogais) conteudo -- Usa 'filter' para manter apenas os caracteres que são vogais e depois 'length' para contá-los.
    
    putStrLn ("O arquivo tem " ++ show numVogais ++ " vogais.") -- Exibe o resultado.

-- Q4. 'contarCaracteres'
-- Conta a ocorrência de um caractere em um arquivo.
contarCaracteres :: IO ()
contarCaracteres = do
    putStrLn "Digite o nome do arquivo:" -- Pede o nome do arquivo.
    nomeArquivo <- getLine -- Lê o nome.
    putStrLn "Digite o caractere a ser contado:" -- Pede o caractere.
    charContar <- getChar -- Lê o caractere.
    
    conteudo <- readFile nomeArquivo -- Lê o conteúdo.
    let numOcorrencias = length $ filter (== charContar) conteudo -- Filtra a string para encontrar apenas o caractere e depois conta.
    
    putStrLn ("O caractere '" ++ [charContar] ++ "' ocorre " ++ show numOcorrencias ++ " vezes no arquivo.") -- Exibe o resultado.

-- Q5. 'juntarArquivos'
-- Cria um novo arquivo com o conteúdo de dois arquivos existentes.
juntarArquivos :: IO ()
juntarArquivos = do
    putStrLn "Digite o nome do primeiro arquivo:" -- Pede o primeiro nome.
    arq1 <- getLine -- Lê o nome.
    putStrLn "Digite o nome do segundo arquivo:" -- Pede o segundo nome.
    arq2 <- getLine -- Lê o nome.
    putStrLn "Digite o nome do arquivo de saída:" -- Pede o nome do arquivo de saída.
    arqSaida <- getLine -- Lê o nome.
    
    conteudo1 <- readFile arq1 -- Lê o conteúdo do primeiro arquivo.
    conteudo2 <- readFile arq2 -- Lê o conteúdo do segundo arquivo.
    
    let conteudoFinal = conteudo1 ++ conteudo2 -- Concatena os dois conteúdos.
    
    writeFile arqSaida conteudoFinal -- Escreve o conteúdo final no novo arquivo.
    
    putStrLn "Arquivos unidos com sucesso!" -- Mensagem de sucesso.

-- Q6. 'acharCidadeMaisPopulosa'
-- Lê um arquivo com cidades e populações, e gera um novo arquivo com a cidade mais populosa.
acharCidadeMaisPopulosa :: IO ()
acharCidadeMaisPopulosa = do
    putStrLn "Digite o nome do arquivo de entrada:" -- Pede o nome do arquivo de entrada.
    arqEntrada <- getLine -- Lê o nome.
    putStrLn "Digite o nome do arquivo de saída:" -- Pede o nome do arquivo de saída.
    arqSaida <- getLine -- Lê o nome.
    
    conteudo <- readFile arqEntrada -- Lê o conteúdo do arquivo de entrada.
    let linhas = lines conteudo -- Divide o conteúdo em linhas.
    let cidades = map parseLinha linhas -- Usa 'map' para aplicar a função 'parseLinha' a cada linha, convertendo-as em pares (cidade, população).
    let cidadeMaisPopulosa = maximumBy (comparing snd) cidades -- 'maximumBy' encontra o maior elemento da lista com base na comparação do segundo elemento da tupla (a população).
    
    writeFile arqSaida (fst cidadeMaisPopulosa ++ " " ++ show (snd cidadeMaisPopulosa)) -- Escreve a cidade e a população mais alta no arquivo de saída.
    
    putStrLn "Arquivo de saída gerado com a cidade mais populosa." -- Mensagem de sucesso.

-- Função auxiliar para converter "NomeDaCidade 123456" em ("NomeDaCidade", 123456).
parseLinha :: String -> (String, Int)
parseLinha linha =
    let partes = words linha -- Divide a linha em palavras.
        nome = unwords (init partes) -- Junta todas as palavras, exceto a última, para formar o nome da cidade.
        populacao = read (last partes) :: Int -- Converte a última palavra para um número inteiro.
    in (nome, populacao) -- Retorna a tupla.
 
-- Q7. 'cadastrarContatos'
-- Permite ao usuário cadastrar nomes e telefones em um arquivo.
cadastrarContatos :: IO ()
cadastrarContatos = do
    putStrLn "Digite o nome do arquivo para cadastro:" -- Pede o nome do arquivo.
    nomeArquivo <- getLine -- Lê o nome.
    
    putStrLn "Digite o nome e o telefone (telefone '0' para parar):" -- Informa o usuário.
    handle <- openFile nomeArquivo WriteMode -- Abre o arquivo em modo de escrita.
    escreverContatos handle -- Chama a função auxiliar que lida com a entrada de dados.
    hClose handle -- Fecha o arquivo.
    
    putStrLn "Cadastro finalizado!" -- Mensagem de sucesso.
    where
      escreverContatos handle = do
        putStr "Nome: " -- Pede o nome.
        nome <- getLine -- Lê o nome.
        putStr "Telefone: " -- Pede o telefone.
        telefone <- getLine -- Lê o telefone.
        
        if telefone == "0" -- Se o telefone for '0', a recursão para.
          then return ()
          else do
            hPutStrLn handle (nome ++ "," ++ telefone) -- Escreve a linha no arquivo no formato "nome,telefone".
            escreverContatos handle -- Chama recursivamente.
            
-- Q8. 'substituirVogais'
-- Cria um novo arquivo substituindo as vogais por '*'.
substituirVogais :: IO ()
substituirVogais = do
    putStrLn "Digite o nome do arquivo de entrada:" -- Pede o nome do arquivo de entrada.
    arqEntrada <- getLine -- Lê o nome.
    putStrLn "Digite o nome do arquivo de saída:" -- Pede o nome do arquivo de saída.
    arqSaida <- getLine -- Lê o nome.
    
    conteudo <- readFile arqEntrada -- Lê o conteúdo.
    let novoConteudo = map substituirVogal conteudo -- Usa 'map' para aplicar a função 'substituirVogal' a cada caractere.
    
    writeFile arqSaida novoConteudo -- Escreve o novo conteúdo no arquivo de saída.
    
    putStrLn "Vogais substituídas com sucesso!" -- Mensagem de sucesso.
    where
      substituirVogal c
        | toLower c `elem` "aeiou" = '*' -- Se o caractere, em minúsculo, estiver na string de vogais, ele é substituído por '*'.
        | otherwise = c -- Caso contrário, o caractere é mantido.
        
-- Q9. 'converterParaMaiusculas' 
-- Cria um novo arquivo com o mesmo conteúdo do original, mas em maiúsculas.
converterParaMaiusculas :: IO ()
converterParaMaiusculas = do
    putStrLn "Digite o nome do arquivo de entrada:" -- Pede o nome do arquivo de entrada.
    arqEntrada <- getLine -- Lê o nome.
    putStrLn "Digite o nome do arquivo de saída:" -- Pede o nome do arquivo de saída.
    arqSaida <- getLine -- Lê o nome.
    
    conteudo <- readFile arqEntrada -- Lê o conteúdo.
    let conteudoMaiusculo = map toUpper conteudo -- Usa 'map' para converter cada caractere para maiúscula.
    
    writeFile arqSaida conteudoMaiusculo -- Escreve o novo conteúdo no arquivo de saída.
    
    putStrLn "Arquivo convertido para maiúsculas com sucesso!" -- Mensagem de sucesso.

-- Q10. 'contarPalavras'
-- Conta quantas vezes uma palavra específica aparece em um arquivo, de forma insensível a maiúsculas e minúsculas.
contarPalavras :: IO ()
contarPalavras = do
    putStrLn "Digite o nome do arquivo:" -- Pede o nome do arquivo.
    nomeArquivo <- getLine -- Lê o nome.
    putStrLn "Digite a palavra a ser contada:" -- Pede a palavra.
    palavra <- getLine -- Lê a palavra.
    
    conteudo <- readFile nomeArquivo -- Lê o conteúdo.
    let palavrasNoArquivo = map (map toLower . filter isLetter) (words conteudo) -- 'words' divide o texto em palavras. 'filter isLetter' remove pontuação. 'map toLower' converte tudo para minúsculas.
    let palavraProcurada = map toLower palavra -- Converte a palavra a ser buscada para minúsculas.
    
    let numOcorrencias = length $ filter (== palavraProcurada) palavrasNoArquivo -- Filtra as palavras que correspondem à palavra procurada e conta o número de ocorrências.
    
    putStrLn ("A palavra '" ++ palavra ++ "' ocorre " ++ show numOcorrencias ++ " vezes no arquivo.") -- Exibe o resultado.

-- Q11. 'escreverInteiros' e 'lerInteiros'
-- Funções para escrever e ler uma lista de inteiros em/de um arquivo.
-- 'escreverInteiros'
-- Escreve uma lista de inteiros em um arquivo, um por linha.
escreverInteiros :: String -> [Int] -> IO ()
escreverInteiros nomeArquivo lista = do
    let conteudo = intercalate "\n" (map show lista) -- 'map show lista' converte cada número para uma string. 'intercalate "\n"' junta as strings com quebras de linha entre elas.
    writeFile nomeArquivo conteudo -- Escreve a string formatada no arquivo.

-- 'lerInteiros'
-- Lê um arquivo e devolve uma lista de inteiros.
lerInteiros :: String -> IO [Int]
lerInteiros nomeArquivo = do
    conteudo <- readFile nomeArquivo -- Lê o conteúdo do arquivo.
    let linhas = lines conteudo -- Divide o conteúdo em uma lista de strings.
    let listaDeInteiros = map read linhas :: [Int] -- Usa 'map read' para converter cada string da lista de volta para um número inteiro.
    return listaDeInteiros -- Retorna a lista de inteiros.

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
