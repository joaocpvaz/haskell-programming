-- [2 * x | x <- ex]
-- [x | x <- [1,2,3,4], mod x 2 == 0]

somaTuplas :: [(Int, Int)] -> [Int]
somaTuplas l = [ a+b | (a,b) <- l]

addOrdPairs :: [(Int, Int)] -> [Int]
addOrdPairs l=[a+b | (a,b) <- l, a<b]

filterLC :: (a->Bool) -> [a] -> [a]
filterLC f l = [x | x <- l, f x]

mapL :: (a->b) -> [a] -> [b]
mapL f l = [f x | x <- l]

removeEspacos :: String -> String
removeEspacos l = [x | x <- l, x /= ' ']

sings :: [[a]] -> [a]
sings l = [x | (x:xs) <- l, (length (x:xs)) == 1 ]

matches :: Int -> [Int] -> [Int]
matches a l = [x | x <- l, x == a]

--elemento :: Int -> [Int] -> Bool ISSO RETORNA UM VETOR, COMO QUE FAZ PRA RETORNAR APENAS UM BOOL JESUS
--elemento a l@(z:zs) = [ x | x <- l, (matches a l) /= [] ]

divisores :: Integer -> [Integer]
divisores v = [x | x <- (divisoresAux v), mod v x == 0 ]

divisoresAux :: Integer -> [Integer]
divisoresAux 0 = []
divisoresAux v = divisoresAux (v-1) ++ [v]

isPrime :: Integer -> Bool
isPrime 1 = True
isPrime v = length (divisores v) == 2

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (a:l) = quickSort (menores a l) ++ [a] ++ quickSort (maiores a l)

maiores :: Ord a => a -> [a] -> [a]
maiores a l = [ x | x <- l , x > a]

menores :: Ord a => a -> [a] -> [a]
menores a l = [ x | x <- l , x < a]



