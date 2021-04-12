main :: IO()
main = do
 putStrLn "Qual o seu nome?"
 nome <- getLine
 putStrLn(reverse nome)

leNomeESobrenome :: IO String
leNomeESobrenome = do
 putStrLn "Qual o seu nome?"
 nome <- getLine
 putStrLn "Qual o seu sobrenome?"
 sob <- getLine
 return (nome ++ " " ++ sob)

main1 :: IO()
main1 = do
 resp <- leNomeESobrenome
 putStrLn resp

iSort ::(Ord a) => [a]->[a]
iSort [] = []
iSort (x:xs) = ins x(iSort xs)

ins :: (Ord a) => a -> [a] -> [a]
ins a [] = [a]
ins a(x:xs)
 | a <= x = a : x : xs
 | otherwise = x : ins a xs

main2 :: IO()
main2 = do
 nomes <- leNomes
 putStr ((foldr1 (++) . map (++"\n") . iSort) nomes) 

-- foldr1 (++) (map(++"\n")(iSort nomes))
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

leNomes :: IO [String]
leNomes = do
 nome <- getLine
 if (nome == "")
    then return []
    else do
          nomes <- leNomes
          return ([nome] ++ nomes)
