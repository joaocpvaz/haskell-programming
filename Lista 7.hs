-- 6.1 calcular o tamanho de uma lista usando mapint e foldint

times2 n = 2*n
times3 n = 3*n

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (a:x) = f a : mapInt f x

vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 1
vendas 2 = 8
vendas 3 = 5
vendas 4 = 2
vendas _ = 3

total :: (Int -> Int) -> Int -> Int
total f 0 = 0
total f a = f a + total f (a-1)

foldIntR :: (Int -> Int -> Int) -> [Int] -> Int
foldIntR f [] = error "Nao pode"
foldIntR f [a] = a
foldIntR f (x:xs) = f x (foldIntR f xs)

soma :: Int -> Int -> Int
soma x y = x + y

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs)
 |f x = x : filterString f xs
 |otherwise = filterString f xs

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

somaQuadrados :: [Int] -> Int
somaQuadrados 0 = 0
somaQuadrados (x:xs) = foldIntR soma (mapInt times2 x)



