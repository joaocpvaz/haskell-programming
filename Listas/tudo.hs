--putStr "Joao\nVaz\nChagas"
-- "Joao" ++ "Vaz" = "JoaoVaz"
-- fazer putStr(tabela 3) fazer funçao tabela
--tabela :: Int->String
--show 3 -> recebe e transforma em string

type Pessoa = (String, String, Int)

joao :: Pessoa
joao = ("Joao Silva","222-2222", 17)

nome :: Pessoa -> String
nome (n,t,i) = n

telefone :: Pessoa -> String
telefone (n,t,i) = t

idade::Pessoa -> Int
idade (n,t,i) = i

adicionaTupla :: (Int, Int, Int) -> Int
adicionaTupla (a,b,c) = a + b + c 

--shift :: ((Int, Int), Int) -> (Int, (Int, Int))
--shift (a,b) c = a 

--minEmax :: Int -> Int -> Int -> (Int, Int)
--minEmax a b c = ((max(max(a b) c)), (min(min(a b) c)))

-- ['a','b','c'] == "abc" retorn true
-- [1..7] returna 1 à 7
-- [0,2 .. 10]
-- 1 : [2,3]
--'a' : "bc"
-- 1 : 2 : [] == [1,2]
-- (:) :: t -> [t] -> [t] diz que não pode fazer 'a' : [2,3]
-- (++) :: [a] -> [a] -> [a] [1,2,3] ++ [4,5,6]
-- : insere no fim de lista
-- take 2 [1,2,3,4]

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x(iSort xs)

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a(x:xs)
 | a <= x = a:x:xs
 | otherwise = x : ins a xs

-- insert sem repetiçoes
iSort1 :: [Int] -> [Int]
iSort1 [] = []
iSort1 (x:xs) = ins1 x(iSort1 xs)

ins1 :: Int -> [Int] -> [Int]
ins1 a [] = [a]
ins1 a(x:xs)
 | a == x = a:xs
 | a <= x = a:x:xs
 | otherwise = x : ins1 a xs

--insert em decrescente

iSort2 :: [Int] -> [Int]
iSort2 [] = []
iSort2 (x:xs) = ins2 x(iSort2 xs)

ins2 :: Int -> [Int] -> [Int]
ins2 a [] = [a]
ins2 a(x:xs)
 | a >= x = a:x:xs
 | otherwise = x : ins2 a xs

minEmax :: [Int] -> (Int,Int)
minEmax [] = (0,0)
minEmax l = (head(iSort l), head(reverse(iSort l))) 

--qs :: [Int] -> [Int]
--qs [] = []
--qs(x:xs) = qs(menores x xs) ++ [x] ++ qs(maiores x xs)

--menores :: Int -> [Int]

--mediaLista :: [Int] -> Float
--mediaLista [] = 0
--mediaLista [a] = a
--mediaLista l = (fromIntegral(somaElem l))/(fromIntegral(tamanho l))





















