retornaUltimo :: [c] -> c
retornaUltimo [] = error "lista vazia"
retornaUltimo [a] = a
retornaUltimo (x:xs) = retornaUltimo xs

pegaPosicao :: Int -> [Int] -> Int
pegaPosicao 1 (x:xs) = x
pegaPosicao a (x:xs) = pegaPosicao (a-1) (xs)

pegaLista :: Int -> [Int] -> [Int]
pegaLista _ [] = []
pegaLista 1 (x:xs) = [x]
pegaLista a (x:xs) = x : pegaLista (a-1) xs

retira :: Int -> [Int] -> [Int]
retira _ [] = []
retira 0 l = l
retira a (x:xs) = retira (a-1) (xs) 

mediaLista :: [Int] -> Float
mediaLista [] = 0
mediaLista [a] = fromIntegral(a)
mediaLista l = (fromIntegral(somaElem l))/(fromIntegral(tamanho l))

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

somaElem :: [Int] -> Int
somaElem [] = 0
somaElem (x:xs) = x + somaElem xs

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores _ [] = []
pegaMaiores a (x:xs)
 |a < x = x : pegaMaiores a xs
 |otherwise = pegaMaiores a xs

contaMaiores :: Int -> [Int] -> Int
contaMaiores _ [] = 0
contaMaiores a (x:xs)
 |a < x = x + contaMaiores a xs
 |otherwise = contaMaiores a xs

concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena [] l = l
concatena l [] = l
concatena (m:xs) l = (m:xs) ++ l

intercala :: [Int] -> [Int] -> [Int]
intercala _ [] = []
intercala [] _ = []
intercala (x:xs) (z:zs) = x : z : intercala xs zs

compress :: [Char] -> [Char]
compress [] = []
compress [a] = [a]
compress (x:z:xs)
 |x == z = compress (z:xs)
 |otherwise = x : compress (z:xs)

pack :: String -> [String]
pack [] = []
pack l@(x:xs) = (getSub x l) : pack (dropSub x l)

getSub :: Char -> String -> String
getSub a [] = []
getSub a (x:xs)
 |a == x = a : getSub a xs
 |otherwise = []

dropSub :: Char -> String -> String
dropSub a [] = []
dropSub a (x:xs)
 |a == x = dropSub a xs
 |otherwise = x:xs

-- ===============================================================
-- ENCODE 
 
encode :: String -> [(Int,Char)]
encode [] = []
encode l@(x:xs) = getTupla 0 x l : encode (dropTupla x l)

getTupla :: Int -> Char -> String -> (Int,Char)
getTupla  _ a [] = (0,a)
getTupla  i a [c] = (i+1,c)
getTupla i a (x:xs)
 |a == x = getTupla (i+1) a xs
 |otherwise = (i,a)
 
dropTupla :: Char -> String -> String
dropTupla a [] = []
dropTupla a (x:xs)
 |a == x = dropTupla a xs
 |otherwise = (x:xs)

-- ENCODE
-- =============================================================== 
 
dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: Int -> [Char] -> [Char]
repli _ [] = []
repli 0 _ = []
repli a (x:xs) = x : repli (a-1) [x] ++ repli a (xs)

-- =============================================================== 
-- DROBEVERY

drobEvery :: Int -> [Char] -> [Char]
drobEvery _ [] = []
drobEvery 1 l = []
drobEvery _ [a] = [a] 
drobEvery a l@(x:xs) = drobEveryAux a l ++ drobEvery a (drobEveryAux1 a l)

drobEveryAux :: Int -> [Char] -> [Char]
drobEveryAux _ [] = []
drobEveryAux 1 (x:xs) = []
drobEveryAux a (x:xs) = x : drobEveryAux (a-1) xs 

drobEveryAux1 :: Int -> [Char] -> [Char]
drobEveryAux1 _ [] = []
drobEveryAux1 1 (x:xs) = xs
drobEveryAux1 a (x:xs) = drobEveryAux1 (a-1) (xs)

-- =============================================================== 
-- SPLIT

split :: Int -> String -> (String,String)
split _ [] = ([],[])
split 0 l = ("",l)
split a l@(x:xs) = (getStr a l,dropStr a l)

getStr :: Int -> String -> String
getStr _ [] = []
getStr 0 l = []
getStr a (x:xs) = x : getStr (a-1) xs

dropStr :: Int -> String -> String
dropStr _ [] = []
dropStr 0 l = l
dropStr a (x:xs) = dropStr (a-1) xs

-- =============================================================== 
-- SLICE

slice :: Int -> Int -> [Int] -> [Int]
slice _ _ [] = []
slice a b (x:xs) = sliceOrg (b) (sliceAux a (x:xs))

sliceAux :: Int -> [Int] -> [Int]
sliceAux 1 l = l
sliceAux a (x:xs) = sliceAux (a-1) xs

sliceOrg :: Int -> [Int] -> [Int]
sliceOrg 1 l = []
sliceOrg a (x:xs) = x : sliceOrg (a-1) xs

-- =============================================================== 
-- DUVIDA : POR QUE EU NÃO POSSO COLOCAR '' COMO FAÇO COM ""?
-- REMOVEAT

removeAt :: Int -> String -> (Char,String)
removeAt _ [] = (' ',[])
removeAt a l@(x:xs) = (getChart a l , getString a l)

getChart :: Int -> String -> Char
getChart _ [] = ' '
getChart 1 (x:xs) = x
getChart a (x:xs) = getChart (a-1) xs

getString :: Int -> String -> String
getString _ [] = []
getString 1 (x:xs) = xs
getString a (x:xs) = x : getString (a-1) xs 

-- =============================================================== 