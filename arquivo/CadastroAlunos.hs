-- Autores: Wanderlucio Williaan Lopes Souza, Matheus Henrique da Cruz Souza

import System.Directory
import System.IO
import Data.Char

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

data Aluno = Aluno { nome :: String, cpf :: String, nota :: Float } deriving ( Eq, Ord, Show, Read )

main :: IO ()
main = do      
    putStrLn ""
    putStrLn "Escolha uma das opcoes abaixo e em seguida tecle [ENTER]:"
    putStrLn ""
    putStrLn "Opcao         Descricao"
    putStrLn ""
    putStrLn "  0 ......... Listar Cadastrados"
    putStrLn "  1 ......... Cadastrar Aluno"
    putStrLn "  2 ......... Gerar Relatorio"
    putStrLn "  3 ......... Buscar por nome"
    putStrLn "  4 ......... Buscar por CPF"
    putStrLn "  5 ......... Media da Turma"
    putStrLn "  6 ......... Excluir Cadastro"
    putStrLn "  7 ......... Sair do Sistema"
    
    putStrLn ""
    putStr "Digite uma opcao: "
    line <- getLine
    putStrLn ""

    if (verificaEntrada line) then
        if (line == "0") then do
            listarTodos
            putStrLn ""
            main
        else if (line == "1") then do
            cadastrar
            putStrLn ""
            main
        else if (line == "2") then do
            gerarRelatorio
            putStrLn ""
            main
        else if (line == "3") then do
            aluno <- buscarPorNome
            putStr "Aluno encontrado: "
            print aluno
            putStrLn ""
            main
        else if (line == "4") then do
            aluno <- buscarPorCpf
            putStr "Aluno encontrado: "
            print aluno
            putStrLn ""
            main
        else if (line == "5") then do
            m <- mediaTurma 
            putStr "Media da turma: "
            print m
            putStrLn ""
            main
        else if (line == "6") then do
            m <- excluir 
            putStrLn ""
            main
        else do
            sair
    else do
        putStrLn "################## OPCAO INVALIDA! ##################"
        putStrLn ""
        main

verificaEntrada :: String -> Bool
verificaEntrada "0" = True
verificaEntrada "1" = True
verificaEntrada "2" = True
verificaEntrada "3" = True
verificaEntrada "4" = True
verificaEntrada "5" = True
verificaEntrada "6" = True
verificaEntrada "7" = True
verificaEntrada _ = False

{-- listarTodos --}
listarTodos :: IO ()
listarTodos = do
    lista <- lerArquivo
    putStrLn "Nome                           CPF                           Nota"
    mapM_ putStrLn (map visualizar lista)

visualizar :: Aluno -> String
visualizar aluno = (nome aluno) ++ "                       " ++ (cpf aluno) ++ "                 " ++ show (nota aluno)
{-- fim listarTodos --}

cadastrar :: IO ()
cadastrar = do
    putStr "Digite CPF: "
    l2 <- getLine
    let cpf = replace l2
    if (validarCpf cpf) then do
        let cpfFormatado = cpfFormat cpf
        putStr "Digite nome: "
        l1 <- getLine
        putStr "Digite Nota: "
        l3 <- lerFloat
        lista <- lerArquivo
        let aluno = Aluno {nome = l1, cpf = cpfFormatado, nota = l3 }
        let alunos = lista ++ [aluno]
        salvarEmArquivo alunos
        putStrLn ""
        putStrLn "Aluno cadastrado!"
    else do
        putStrLn "CPF invalido!"
        putStrLn ""
        cadastrar

buscarPorNome :: IO [Aluno]
buscarPorNome = do
    putStr "Digite nome: "
    l1 <- getLine
    lista <- lerArquivo
    let m = filter (eqNome l1) lista
    return m

eqNome :: String -> Aluno -> Bool
eqNome bNome aluno = bNome == (nome aluno)

buscarPorCpf :: IO [Aluno]
buscarPorCpf = do
    putStr "Digite CPF: "    
    l1 <- getLine
    let cpf = replace l1
    let cpfFormatado = cpfFormat cpf
    lista <- lerArquivo
    let m = filter (eqCpf cpfFormatado) lista
    return m

eqCpf :: String -> Aluno -> Bool
eqCpf bCpf aluno = bCpf == (cpf aluno)

mediaTurma :: IO Float
mediaTurma = do
    lista <- lerArquivo
    let notas = map appenasNotas lista
    let media = (foldr1 (\x y -> (x + y)) notas) / (fromIntegral $ length notas)
    return media

appenasNotas :: Aluno -> Float
appenasNotas aluno = (nota aluno)

--media :: [Aluno] -> Float
--media [] = 0.0
--media (x:xs) = (somatorioMedia (x:xs)) /  (fromIntegral $ length (x:xs))

--somatorioMedia :: [Aluno] -> Float
--somatorioMedia [] = 0.0
--somatorioMedia (x:xs) = (nota x) + (somatorioMedia xs)

excluir :: IO ()
excluir = do
    putStr "Digite CPF: "
    l1 <- getLine
    lista <- lerArquivo
    let novaLista = filter (neCpf l1) lista
    salvarEmArquivo novaLista
    putStrLn ""
    putStrLn "Excluido!"

neNome :: String -> Aluno -> Bool
neNome bNome aluno = bNome /= (nome aluno)

neCpf :: String -> Aluno -> Bool
neCpf bCpf aluno = bCpf /= (cpf aluno)

maiorNota :: IO Float
maiorNota = do
    lista <- lerArquivo
    let notas = map appenasNotas lista
    let maior = foldr1 max notas
    return maior

menorNota :: IO Float
menorNota = do
    lista <- lerArquivo
    let notas = map appenasNotas lista
    let menor = foldr1 min notas
    return menor

sair :: IO ()
sair = do
    putStrLn "Ate mais!"
    putStrLn ""

salvarEmArquivo :: [Aluno] -> IO ()
salvarEmArquivo lista = do
    let nomeArquivo = "banco_alunos.txt"
    writeFile nomeArquivo (show lista)

lerArquivo :: IO [Aluno]
lerArquivo = do
    let nomeArquivo = "banco_alunos.txt"
    file <- doesFileExist nomeArquivo
    if ( file ) then do
        parse <- readFile nomeArquivo
        let lista = (read parse :: [Aluno])
        seq (length parse) (return ())
        return lista
    else do
        writeFile "banco_alunos.txt" ""
        return [] 

gerarRelatorio :: IO ()
gerarRelatorio = do
    lista <- lerArquivo
    media <- mediaTurma
    maior <- maiorNota
    menor <- menorNota
    dataH <- getDate
    let cabecalho1 = "#################################################################"
    let cabecalho2 = "##              RELATORIO ALUNOS           " ++ dataH ++ "          ##"
    let cabecalho3 = "#################################################################\n"
    let cabecalho4 = "Nome                           CPF                           Nota\n"
    let rodape1 = "Media da Turma                                               " ++ show media ++"\n"
    let rodape3 = "Maior Nota                                                   " ++ show maior ++ "\n"
    let rodape2 = "Menor Nota                                                   " ++ show menor ++ "\n"
    let rodape4 = "\n#################################################################"
    let formatarTabela = map saidaRelatorio lista
    let p1 = ( cabecalho1 : cabecalho2 : cabecalho3 : cabecalho4 : formatarTabela)
    let p2 = ("\n"++ rodape1 ++ rodape2 ++ rodape3 ++ rodape4)
    let concat = concatenaElementos p2 p1
    let saida = unlines concat
    writeFile "relatorio_alunos.txt" saida
    putStrLn "Relatorio gerado!"

concatenaElementos :: String -> [String] -> [String]
concatenaElementos elem [] = [elem]
concatenaElementos elem (x:xs) = x : concatenaElementos elem xs

saidaRelatorio :: Aluno -> String
saidaRelatorio aluno = ((nome aluno) ++ "                        " ++ (cpf aluno) ++ "                " ++ (show (nota aluno)) ++ "")

lerFloat :: IO Float
lerFloat = do 
    valor <- getLine
    return (read valor :: Float)

primeirocompararDigitoVerificador :: [Char] -> Int
primeirocompararDigitoVerificador [] = 0
primeirocompararDigitoVerificador (x:xs) = varificaDigito ( 11 - ( mod (somatorioPrimeiroDigito (x:xs) 10) 11 ) )

somatorioPrimeiroDigito :: [Char] -> Int -> Int
somatorioPrimeiroDigito [] n = 0
somatorioPrimeiroDigito (x:xs) n = (digitToInt x * n) + somatorioPrimeiroDigito xs (n-1)

segundocompararDigitoVerificador :: [Char] -> Int
segundocompararDigitoVerificador [] = 0
segundocompararDigitoVerificador (x:xs) = varificaDigito ( 11 - (mod (somatorioSegundoDigito (x:xs) 11) 11) )

somatorioSegundoDigito :: [Char] -> Int -> Int
somatorioSegundoDigito [] n = 0
somatorioSegundoDigito (x:xs) n = (digitToInt x * n) + somatorioSegundoDigito xs (n-1)

{--
Se o resto da divisão em primeiroDigitoVerificador ou  segundoDigitoVerificador for 10 ou 11, 
o dígito verificador será 0; nos outros casos, o dígito verificador é o próprio resto.
--}
varificaDigito :: Int -> Int
varificaDigito digito
    | (digito == 10) || (digito == 11) = 0
    | otherwise = digito

compararDigitoVerificador :: Int -> String -> Int
compararDigitoVerificador _ [] = 0
compararDigitoVerificador n (x:xs)
    | n == 1 = digitToInt ( head ( drop 9 (x:xs) ) )
    | n == 2 = digitToInt (last (x:xs))    
    | otherwise = 0 

validarCpf :: String -> Bool
validarCpf [] = False
validarCpf "00000000000" = False
validarCpf "11111111111" = False
validarCpf "22222222222" = False
validarCpf "33333333333" = False
validarCpf "44444444444" = False
validarCpf "55555555555" = False
validarCpf "66666666666" = False
validarCpf "77777777777" = False
validarCpf "88888888888" = False
validarCpf "99999999999" = False
validarCpf (x:xs)
    | length (x:xs) < 11 || length (x:xs) > 14 = False
    | ( primeirocompararDigitoVerificador ( take 9 (x:xs) ) == (compararDigitoVerificador 1 (x:xs)) ) && ( segundocompararDigitoVerificador ( take 10 (x:xs) ) == (compararDigitoVerificador 2 (x:xs)) ) = True
    | otherwise = False

replace :: String -> String
replace s = filter (/='-') ( filter (/='.') s)

getDate :: IO String
getDate = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    let (year, month, day) = toGregorian $ localDay zoneNow
    let d = show day ++ "/" ++ show month ++ "/" ++ show year
    return d

cpfFormat :: String -> String
cpfFormat (x:xs)
    | (length xs) < 3 = []
    | otherwise = take 11 ( (take 3 (x:xs)) ++ "." ++ cpfFormat (drop 3 (x:xs)) ) ++ "-" ++ drop 9 (x:xs)