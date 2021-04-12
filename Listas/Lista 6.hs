somaTripla :: [(Int,Int,Int)] -> Int
somaTripla [] = 0
somaTripla ((a,b,c):x) = a + b + c + somaTripla x

somaTupla :: [((Int,Int),(Int,Int))] -> Int
somaTupla [] = 0
somaTupla(((a,b),(c,d)):x) = a + b + c + d + somaTupla ((x))

zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [] l = [] 
zipp l [] = []
zipp (x:xs)(z:zs) = ((x,z)) : zipp (xs)(zs)

zip33 :: [Int]->[Int]->[Int] -> [(Int,Int,Int)]
zip33 [] _ _ = []
zip33 _ [] _ = []
zip33 _ _ [] = []
zip33 (x:xs)(z:zs)(w:ws) = ((x,z,w)) : zip33 (xs)(zs)(ws)

unZipp :: [(Int,Int)] -> ([Int], [Int])
unZipp [] = ([],[])
unZipp l@((a,b):x) = ((unZipEsq l) , (unZipDir l)) 

unZipEsq :: [(Int,Int)] -> [Int]
unZipEsq [] = []
unZipEsq ((a,b):xs) = a : unZipEsq xs

unZipDir :: [(Int,Int)] -> [Int]
unZipDir [] = []
unZipDir ((a,b):x) = b : unZipDir x