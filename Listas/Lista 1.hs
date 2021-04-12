osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais w x y z = (w==x) && (x==y) &&(y==z)

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z 
 |(x == y) && (y == z) = 3
 |(x /= y) && (y /= z) = 0
 |otherwise = 2
 
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = (x /= y) && (x /= z)

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == z) && (x == z)

quantosSaoIguais1 :: Int -> Int -> Int -> Int
quantosSaoIguais1 x y z 
 |todosIguais x y z = 3
 |todosDiferentes x y z= 0
 |otherwise = 0

elevadoDois :: Int -> Int
elevadoDois 0 = 0
elevadoDois x = x * x

elevadoQuatro :: Int -> Int
elevadoQuatro 0 = 0
elevadoQuatro x = elevadoDois x * elevadoDois x

vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 2
vendas 2 = 1
vendas 3 = 5
vendas 4 = 10
vendas _ = 4

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n-1)
