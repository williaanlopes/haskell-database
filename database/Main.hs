
module Main where

import System.IO
import Data.Char
import qualified Data.Text as Text
import Aluno
import qualified AlunoDao as AlunoDao
import qualified Util as Util

{-- main --}
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

    if (Util.verificaEntrada line) then
        if (line == "0") then do
            listarTodos
            putStrLn ""
            main
        else if (line == "1") then do
            cadastrar
            putStrLn ""
            main
        else if (line == "2") then 
            putStrLn "Op 2"
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
            excluir 
            putStrLn ""
            main
        else do
            sair
    else do
        putStrLn "################## OPCAO INVALIDA! ##################"
        putStrLn ""
        main
{-- fim main --}

{-- listarTodos --}
listarTodos :: IO ()
listarTodos = do
    alunos <- AlunoDao.listar
    putStrLn "#   Nome                           CPF                           Nota"
    mapM_ putStrLn (map Util.visualizar alunos)
{-- fim listarTodos --}

{-- cadastrar --}
cadastrar :: IO ()
cadastrar = do
    putStr "Digite CPF: "
    l2 <- getLine
    let cpf = Util.replace l2    
    if (Util.validarCpf cpf) then do
        let cpfFormatado = Util.cpfFormat cpf
        putStr "Digite nome: "
        l1 <- getLine
        putStr "Digite Nota: "
        l3 <- Util.lerFloat
        let aluno = Aluno {_id = 0, _nome = Text.pack l1, _cpf = Text.pack cpfFormatado, _nota = l3 }
        AlunoDao.salvar aluno
        main
    else do
        putStrLn "CPF invalido!"
        putStrLn ""
        cadastrar
{-- fim cadastrar --}

{-- buscarPorNome --}
buscarPorNome :: IO [Aluno]
buscarPorNome = do
    putStr "Digite nome: "
    l1 <- getLine
    aluno <- AlunoDao.porNome l1
    return aluno
{-- fim buscarPorNome --}

{-- buscarPorCpf --}
buscarPorCpf :: IO [Aluno]
buscarPorCpf = do
    putStr "Digite CPF: "
    l1 <- getLine
    let rp = Util.replace l1
    let fr = Util.cpfFormat rp
    aluno <- AlunoDao.porCpf fr
    return aluno
{-- fim buscarPorCpf --}

{-- mediaTurma --}
mediaTurma :: IO Float
mediaTurma = do
    lista <- AlunoDao.listar -- de AlunoDao
    let notas = map Util.appenasNotas lista
    let media = (foldr1 (\x y -> (x + y)) notas) / (fromIntegral $ length notas)
    return media
{-- fim mediaTurma --}

{-- excluir --}
excluir :: IO ()
excluir = do
    putStr "Digite CPF: "
    l1 <- getLine
    lista <- AlunoDao.deletarPorCpf l1
    putStrLn "Excluido!"
{-- fim excluir --}

{-- menorNota --}
maiorNota :: IO Float
maiorNota = do
    lista <- AlunoDao.listar
    let notas = map Util.appenasNotas lista
    let maior = foldr1 max notas
    return maior

{-- menorNota --}
menorNota :: IO Float
menorNota = do
    lista <- AlunoDao.listar
    let notas = map Util.appenasNotas lista
    let menor = foldr1 min notas
    return menor
{-- fim menorNota --}

{-- sair --}
sair :: IO ()
sair = do
    putStrLn "Ate mais!"
    putStrLn ""
{-- fim sair --}

teste :: IO ()
teste = do
    l <- getLine
    let p = Util.cpfFormat l
    print p