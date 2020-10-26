-- Autores: Wanderlucio Williaan Lopes Souza, Matheus Henrique da Cruz Souza

import System.IO
import Data.Char

main :: IO ()
main = do
    entrada <- readFile "para_validar.txt"
    let cabecalho = "Relatorio de Validacao de CPF\n"
    let rodape = "\nAutores: Wanderlucio Williaan Lopes Souza, Matheus Henrique da Cruz Souza "
    let lista = lines entrada
    let formatarSaida = map validar lista
    --print cabecalho
    let saida = unlines (cabecalho : formatarSaida ++ [rodape])
    writeFile "validados.txt" saida
    putStrLn "Verifique o arquivo 'validados.txt'"

validar :: String -> String
validar [] = []
validar s 
    | validarCpf (replace s) = s ++ " .......................... valido"
    | otherwise = s ++ " .......................... invalido"
    
primeiroDigitoVerificador :: String -> Int
primeiroDigitoVerificador [] = 0
primeiroDigitoVerificador (x:xs) = varificaDigito ( 11 - ( mod (somatorioPrimeiroDigito (x:xs) 10) 11 ) )

somatorioPrimeiroDigito :: String -> Int -> Int
somatorioPrimeiroDigito [] n = 0
somatorioPrimeiroDigito (x:xs) n = (digitToInt x * n) + somatorioPrimeiroDigito xs (n-1)

segundoDigitoVerificador :: String -> Int
segundoDigitoVerificador [] = 0
segundoDigitoVerificador (x:xs) = varificaDigito ( 11 - (mod (somatorioSegundoDigito (x:xs) 11) 11) )

somatorioSegundoDigito :: String -> Int -> Int
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
    | length (x:xs) > 14 ||  length (x:xs) < 11 = False
    | ( primeiroDigitoVerificador ( take 9 (x:xs) ) == (digitoVerificador 1 (x:xs)) ) && ( segundoDigitoVerificador ( take 10 (x:xs) ) == (digitoVerificador 2 (x:xs)) ) = True
    | otherwise = False

replace :: String -> String
replace [] = []
replace valor = filter (/='-') ( filter (/='.') valor)
-- replace = map (\c -> if c == '.' then ' '; else if c == '-' then ' ' else c)