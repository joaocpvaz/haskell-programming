
-- system.console.ansi
-- syste.io

-- [1,2,3,4]
-- 1:2:3:4:[]

-- Cons concatenador de listas

data Lista a = Cons a (Lista a) | Vazio
 deriving(Eq,Show)

-- lista implementada internamente pelo haskell
ex1 :: Lista Int
ex1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Vazio)))

ex2 :: Lista (Lista Int)
ex2 =  Cons (Cons 1 (Cons 2 Vazio)) (Cons (Cons 3 (Cons 4 Vazio)) (Cons Vazio Vazio))

tamanho :: Lista a -> Int
tamanho Vazio = 0
tamanho (Cons x xs) =  1 + tamanho xs

mapLista :: (a -> b) -> Lista a -> Lista b
mapLista f Vazio = Vazio
mapLista f (Cons x xs) = Cons (f x) (mapLista f xs)

foldr1Lista :: (a -> a -> a) -> Lista a -> a
foldr1Lista f (Cons x Vazio) = x
foldr1Lista f (Cons x xs) = f x (foldr1Lista f xs)

filterLista :: (a -> Bool) -> Lista a -> Lista a
filterLista p Vazio = Vazio
filterLista p (Cons x xs) 
 | p x = Cons x (filterLista p xs)
 | otherwise = filterLista p xs

--reverseLista :: Lista a -> Lista a
--reverseLista (Cons x Vazio) = (Cons x Vazio)
--reverseLista (Cons x xs) = (reverseLista xs)  

-- Cons 1 (Cons 2 (Cons 3 Vazio)) -> Cons 3 (Cons 2 (Cons 1 Vazio))

takeLista :: (Eq a) => a -> Lista a -> Lista a
takeLista a Vazio = Vazio
takeLista z (Cons x Vazio)
 |x == z = (Cons x Vazio)
 |otherwise = Vazio
takeLista z (Cons x xs)
 | z == x = Cons x xs
 |otherwise = takeLista z xs

dropLista ::(Eq a) => a -> Lista a -> Lista a
dropLista a Vazio = Vazio
dropLista a (Cons x Vazio)
  |a == x = Vazio
  |otherwise = (Cons x Vazio)
dropLista a (Cons x xs)
  |a == x = xs
  |otherwise = dropLista a xs

data TuplaPolimorfica a b = Tupla a b
 deriving(Eq,Show)


--pensar no caso de para 0
--myDropp :: Int -> Lista a -> Lista b
--myDropp _ Vazio = Vazio
--myDropp n (Cons x xs) 
-- |n == x = myDropp n xs
-- |otherwise = (Cons x xs)
