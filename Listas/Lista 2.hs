vendas :: Int -> Int
vendas 1 = 16
vendas 2 = 8
vendas 3 = 19
vendas 4 = 1
vendas 5 = 4
vendas 6 = 0 
vendas _ = 48

maxmax :: Int -> Int -> Int -> Int
maxmax  a b c= max(max a b) c

maiorVenda :: Int -> Int
maiorVenda 0 = 0
maiorVenda n = max( vendas n) (maiorVenda (n - 1))

zeroVendas :: Int -> Int
zeroVendas 0
 |vendas 0 == 0 = 0
 |otherwise = -1
zeroVendas n
 |vendas n == 0 = n
 |otherwise = zeroVendas(n-1)
 
vendaIgual :: Int -> Int -> Int
vendaIgual s 0
 |vendas 0 == 0 = 0
 |otherwise = -1
vendaIgual s y 
 |s == vendas y = y
 |otherwise = vendaIgual s (y-1)
 
fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial(x-1)

-- FALTOU A 9, NAO ENTENDI :'(

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
