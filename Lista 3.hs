adicionaTupla :: (Int,Int,Int) -> Int
adicionaTupla (a,b,c) = a + b + c

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a,b),c) = (a,(b,c))

minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = (max(max a b) c,min (min a b) c)

ordenaTuplaFeio :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTuplaFeio(a,b,c)
 |a>b && a>c = (min b c, max b c,a)
 |b>a && b>c = (min a c, max a c,b)
 |c>a && c>b = (min a b, max a b,c)

ordenaTuplaMaisFeioAinda :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTuplaMaisFeioAinda (a, b, c)
 |a<b && a<c && b>a && b>c = (a,c,b)
 |a<b && a<c && c>a && c>b = (a,b,c)
 |b<a && b<c && a>b && a>c = (b,c,a)
 |b<a && b<c && c>a && c>b = (b,a,c)
 |c<b && c<a && a>b && a>c = (c,b,a)
 |c<b && c<a && b>a && b>c = (c,a,b)
 
ordenaTuplaTop :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTuplaTop (a,b,c) = (min(min a b) c,  (max a 0) + (max b 0) + (max c 0) - max(max a b) c - min(min a b) c , max(max a b) c )

zeroVenda :: Int -> (Int, Bool)
zeroVenda 0
  |vendas 0 == 0 = (0, True)
  |otherwise = (-1, False)
zeroVenda n
  |vendas n == 0 = (n, True)
  |otherwise = zeroVenda(n-1)
 
type Livro = (String,String,Int)

joao :: Livro
joao = ("Livro","Joao",21)

livroautor :: Livro -> String
livroautor (t,a,i) = a
 
livrotitulo :: Livro -> String
livrotitulo (t,a,i) = t
 
liroisbn :: Livro -> Int
liroisbn (t,a,i) = i

vendas :: Int -> Int
vendas 1 = 16
vendas 2 = 8
vendas 3 = 19
vendas 4 = 1
vendas 5 = 4
vendas 6 = 0 
vendas _ = 48