dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (a:x) = 2*a : dobraLista x

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho [a] = 1
tamanho (a:x) = 1 + tamanho x

take1 :: Int -> [a] -> [a]
take1 0 _ = []
take1 _ [] = []
take1 n (a:x) = a : take(n-1) x

produtoLista :: [Int] -> Int
produtoLista [] = 0
produtoLista [a] = a
produtoLista (a:x) = a * produtoLista x

andLista :: [Bool] -> Bool
andLista [] = False
andLista (a:x) = a && andLista x

concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (a:xs) = a ++ concatLista xs

inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:x) = inverteLista x ++ [a]

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:x) = a + somaLista x