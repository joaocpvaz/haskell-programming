palindromo :: String -> Bool
palindromo l = (reverse l) == l

verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo a b c = ((a+b) > c) || ((b+c)>a) || ((a+c)>b)

potencia :: Int -> Int -> Int
potencia b 0 = 1
potencia b e = b * potencia b (e-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 

isPrime :: Int -> Bool
isPrime a = 


