module Util where

import System.IO
import Data.Char
import Aluno
import qualified Data.Text as Text

{-- verificaEntrada --}
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
{-- fim verificaEntrada --}

appenasNotas :: Aluno -> Float
appenasNotas aluno = (_nota aluno)

visualizar :: Aluno -> String
visualizar aluno = show (_id aluno) ++ "   " ++ Text.unpack (_nome aluno) ++ "                     " ++ Text.unpack (_cpf aluno) ++ "               " ++ show (_nota aluno)

lerFloat :: IO Float
lerFloat = do 
    valor <- getLine
    return (read valor :: Float)    

replace :: String -> String
replace s = filter (/='-') ( filter (/='.') s)

primeiroDigitoVerificador :: [Char] -> Int
primeiroDigitoVerificador [] = 0
primeiroDigitoVerificador (x:xs) = varificaDigito ( 11 - ( mod (somatorioPrimeiroDigito (x:xs) 10) 11 ) )

somatorioPrimeiroDigito :: [Char] -> Int -> Int
somatorioPrimeiroDigito [] n = 0
somatorioPrimeiroDigito (x:xs) n = (digitToInt x * n) + somatorioPrimeiroDigito xs (n-1)

segundoDigitoVerificador :: [Char] -> Int
segundoDigitoVerificador [] = 0
segundoDigitoVerificador (x:xs) = varificaDigito ( 11 - (mod (somatorioSegundoDigito (x:xs) 11) 11) )

somatorioSegundoDigito :: [Char] -> Int -> Int
somatorioSegundoDigito [] n = 0
somatorioSegundoDigito (x:xs) n = (digitToInt x * n) + somatorioSegundoDigito xs (n-1)

{--
Se o resto da divisão em primeiroDigitoVerificador ou  segundoDigitoVerificador calculado 
for 10 ou 11, o dígito verificador será 0; nos outros casos, o dígito verificador é o próprio resto.
--}
varificaDigito :: Int -> Int
varificaDigito digito
    | (digito == 10) || (digito == 11) = 0
    | otherwise = digito

digitoVerificador :: Int -> [Char] -> Int
digitoVerificador _ [] = 0
digitoVerificador n (x:xs)
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
    | ( primeiroDigitoVerificador ( take 9 (x:xs) ) == (digitoVerificador 1 (x:xs)) ) && ( segundoDigitoVerificador ( take 10 (x:xs) ) == (digitoVerificador 2 (x:xs)) ) = True
    | otherwise = False

cpfFormat :: String -> String
cpfFormat (x:xs)
    | (length xs) < 3 = []
    | otherwise = take 11 ( (take 3 (x:xs)) ++ "." ++ cpfFormat (drop 3 (x:xs)) ) ++ "-" ++ drop 9 (x:xs)
