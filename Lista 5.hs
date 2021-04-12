membro :: [Int] -> Int -> Bool
membro [] a = False
membro (x:xs) a
 |a == x = True
 |otherwise = membro xs a
 
membroNum :: [Int] -> Int -> Int
membroNum [] a = 0
membroNum (x:xs) a
 |x == a = 1 + membroNum xs a
 |otherwise = membroNum xs a
 
membro1 :: [Int] -> Int -> Bool
membro1 [] a = False
membro1 l@(x:xs) a
 |membroNum l a == 0 = False
 |otherwise = True 

unico :: [Int] -> [Int]
unico [] = []
unico l = unico2 l l

unico2 :: [Int] -> [Int] -> [Int]
unico2 [] _ = []
unico2 _ [] = []
unico2 (x:xs) l 
 |membroNum l x == 1 = x : unico2 xs l
 |otherwise = unico2 xs l

membroSort :: [Int] -> Int -> Bool
membroSort [] _ = False
membroSort l@(x:xs) a = membroSortAux (iSort l) a

membroSortAux :: [Int] -> Int -> Bool
membroSortAux [] _ = False
membroSortAux (x:xs) a
 | a < x = False
 | a > x = membroSortAux xs a
 | otherwise = True

-- ==============================================

-- INSERT NORMAL, SEM REQUISITOS
iSort :: [Int]->[Int]
iSort [] = []
iSort (x:xs) = ins x(iSort xs)

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a(x:xs)
 | a <= x = a : x : xs
 | otherwise = x : ins a xs

-- ==============================================

-- INSERT SEM REPETIÇÕES
iSort1 :: [Int]->[Int]
iSort1 [] = []
iSort1 (x:xs) = ins1 x(iSort1 xs)

ins1 :: Int -> [Int] -> [Int]
ins1 a [] = [a]
ins1 a(x:xs)
 | a == x = a:xs
 | a <= x = a:x:xs
 | otherwise = x : ins1 a xs 
 
-- =============================================

-- INSERT ORGANIZANDO DE FORMA DECRESCENTE
iSort2 :: [Int]->[Int]
iSort2 [] = []
iSort2 (x:xs) = ins2 x(iSort2 xs)

ins2 :: Int -> [Int] -> [Int]
ins2 a [] = [a]
ins2 a(x:xs)
 | a >= x = a : x : xs
 | otherwise = x : ins2 a xs 
 
 -- =============================================
