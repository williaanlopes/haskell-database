{-# LANGUAGE OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module AlunoDao where

import Database.MySQL.Base
import Database.MySQL.Nem
import qualified Data.Text as Text
import qualified System.IO.Streams as Streams
import Aluno as Aluno
--import Result as Result

{--
data OK = OK
    { okAffectedRows :: !Int      -- ^ affected row number
    , okLastInsertID :: !Int      -- ^ last insert's ID
    , okStatus       :: !Word16
    , okWarningCnt   :: !Word16
    } deriving (Show, Eq)
--}

dataFromList [MySQLInt32 a, MySQLText b, MySQLText c, MySQLFloat d] = Aluno (fromIntegral a) (b) (c) (d :: Float)
dataFromList _ = error "*** Exception: dataFromList: dados de entrada invalidos"

{-- Listar Todos --}
listar :: IO [Aluno] 
listar = do
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    (defs, is) <- query_ conn "SELECT * FROM aluno"
    l <- Streams.toList is
    let p = map dataFromList l
    return p

{-- Listar por codigo --}
porCodigo  :: Integer -> IO [Aluno] 
porCodigo codigo = do
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    s <- prepareStmt conn "SELECT id, nome, cpf, nota FROM aluno WHERE id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U (fromIntegral codigo)]
    l <- Streams.toList is
    let p = map dataFromList l
    return p
{-- --}

{-- Listar por codigo --}
porNome :: String -> IO [Aluno] 
porNome nome = do
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    s <- prepareStmt conn "SELECT id, nome, cpf, nota FROM aluno WHERE nome = ?"
    (defs, is) <- queryStmt conn s [MySQLText (Text.pack nome)]
    l <- Streams.toList is
    let p = map dataFromList l
    return p
{-- --}

{-- Listar por codigo --}
porCpf :: String -> IO [Aluno] 
porCpf cpf = do
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    ps <- prepareStmt conn "SELECT id, nome, cpf, nota FROM aluno WHERE cpf = ?"
    (defs, is) <- queryStmt conn ps [MySQLText (Text.pack cpf)]
    l <- Streams.toList is
    let p = map dataFromList l
    return p
{-- --}

{-- Salvar --} 
salvar :: Aluno -> IO Bool
salvar aluno = do
    --let aluno = Aluno {_id = 0, _nome = "Teste 3", _cpf = "789", _nota = 8.4}
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    ps <- prepareStmt conn "INSERT INTO aluno (nome, cpf, nota) VALUES (?, ?, ?)"
    rs <- executeStmt conn ps [MySQLText (_nome aluno), MySQLText (_cpf aluno), MySQLFloat (_nota aluno)]
    if ((okStatus rs) == 2) then do
        return True
    else do
        return False

{-- Atualizar --}
atualizar :: Aluno -> IO Bool 
atualizar aluno = do
    lista <- porCodigo (_id aluno)
    let aluno2 = head lista
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    ps <- prepareStmt conn "UPDATE aluno SET nome = ?, cpf = ?, nota = ? WHERE cpf = ?"
    rs <- executeStmt conn ps [MySQLText (_nome aluno), MySQLText (_cpf aluno), MySQLFloat (_nota aluno), MySQLInt32U (fromIntegral (_id aluno2))]
--    r <- executeStmt conn s [MySQLText "Teste 4", MySQLText "321", MySQLFloat (_nota aluno), MySQLInt32U 4]
    if ((okStatus rs) == 2) then do
        return True
    else do
        return False

{-- Deletar --}
deletar :: Integer -> IO Bool 
deletar codigo = do
    lista <- porCodigo codigo
    let aluno = head lista
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    ps <- prepareStmt conn "DELETE FROM aluno WHERE id = ?"
    rs <- executeStmt conn ps [MySQLInt32U (fromIntegral codigo)]
    if ((okStatus rs) == 2) then do
        return True
    else do
        return False

deletarPorCpf :: String -> IO Bool 
deletarPorCpf cpf = do
    lista <- porCpf cpf
    let aluno = head lista
    conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "root", ciDatabase = "haskell"}
    ps <- prepareStmt conn "DELETE FROM aluno WHERE cpf = ?"
    rs <- executeStmt conn ps [MySQLText (Text.pack cpf)]
    if ((okStatus rs) == 2) then do
        return True
    else do
        return False
