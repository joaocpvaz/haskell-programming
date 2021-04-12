-- >:t  map2
concatLista :: [[a]] -> [a]
concatLista [] = []
concatLista (a:x) = a ++ (concatLista x)
