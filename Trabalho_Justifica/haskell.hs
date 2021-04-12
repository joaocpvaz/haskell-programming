justifica :: String -> String
justifica [] = []
justifica l = justificarLinhas (removeUltimaLinha (separaLinhas l)) (tamanhoMaiorLinha (separaLinhas l)) ++ pegaUltimaLinha (separaLinhas l) ++ "\n"

removeUltimaLinha :: [String] -> [String]
removeUltimaLinha [] = []
removeUltimaLinha [x] = []
removeUltimaLinha (x:xs) = x : removeUltimaLinha xs

pegaUltimaLinha :: [String] -> String
pegaUltimaLinha [] = []
pegaUltimaLinha [x] = x
pegaUltimaLinha (x:xs)= pegaUltimaLinha xs

-- O "n" � O TAMANHO DA MAIOR LINHA HEHEHE
justificarLinhas :: [String] -> Int -> String
justificarLinhas [] n = []
justificarLinhas (x:xs) n = justificarUmaLinha x n ++ "\n" ++ justificarLinhas xs n  

justificarUmaLinha :: String -> Int -> String
justificarUmaLinha [] n = []
justificarUmaLinha l n = insereEspacos (div (n - (tamanhoLinha l)) (numeroEspacos l)) (mod (n - (tamanhoLinha l)) (numeroEspacos l)) l

testando :: String -> Int -> Int
testando [] n = 0
testando l n = div (n - (tamanhoLinha l)) (numeroEspacos l)

-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas l
 |dropWhile (/= '\n') l /= [] = takeWhile (/= '\n') l : separaLinhas (tail(dropWhile (/= '\n') l))
 |otherwise = [l]

--separaPalavras :: String -> [String]
--separaPalavras [] = []
--separaPalavras l
-- |dropWhile (/= ' ') l /= [] = takeWhile (/= ' ') l : separaPalavras (tail(dropWhile (/= ' ') l))
-- |otherwise = [l]

-- ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ VERIFICAÇÃO DE MAIOR LINHA

tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha [] = error "Lista Vazia"
tamanhoMaiorLinha l = verificaMaiorLinha (juntaTamanhos tamanhoLinha l)

verificaMaiorLinha :: [Int] -> Int
verificaMaiorLinha [a] = a
verificaMaiorLinha (a:b:xs)
 | a >= b = verificaMaiorLinha (a:xs)
 | otherwise = verificaMaiorLinha (b:xs)

juntaTamanhos :: (String -> Int) -> [String] -> [Int]
juntaTamanhos f [] = []
juntaTamanhos f (x:xs) = f x : juntaTamanhos f xs

tamanhoLinha :: String -> Int
tamanhoLinha [] = 0
tamanhoLinha (x:xs) = 1 + tamanhoLinha xs  

-- ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ VERIFICAÇÃO DE MAIOR LINHA

insereEspacos :: Int -> Int -> String -> String
insereEspacos _ _ [] = []
insereEspacos espacos resto l 
 |resto /= 0 = takeWhile (/= ' ') l ++ preencheEspacos espacos ++ "  " ++ insereEspacos espacos (resto-1) (drop 1(dropWhile (/= ' ') l))
 |(dropWhile (/= ' ') l) == [] = l
 |otherwise = takeWhile (/= ' ') l ++ preencheEspacos espacos ++ " " ++ insereEspacos espacos 0 (drop 1(dropWhile (/= ' ') l))
 
preencheEspacos :: Int -> String
preencheEspacos 0 = []
preencheEspacos n = ' ' : preencheEspacos (n-1)

numeroEspacos :: String -> Int
numeroEspacos [] = 0
numeroEspacos (x1:xs)
 |x1 == ' '  = 1 + numeroEspacos xs
 |otherwise = numeroEspacos xs
