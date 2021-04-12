data Arvore a = Folha a | No a (Arvore a) (Arvore a)
 deriving(Eq,Show)

arv1 :: Arvore Int
arv1 = No 3 (No 2 (Folha 4)(Folha 5)) (Folha 6)

arv2 :: Arvore Char
arv2 = No 'b' (Folha 'c') (No 'd' (Folha 'e')(No 'f' (Folha 'g')(Folha 'h')))
 
somaArv :: Arvore Int -> Int
somaArv (Folha v) = v
somaArv (No v a1 a2) = v + somaArv a1 + somaArv a2

multArv :: Arvore Int -> Arvore Int
multArv (Folha v) = Folha (v*2)
multArv (No v a1 a2) = No (v*2) (multArv a1) (multArv a2)

contaArv :: Arvore Int -> Int
contaArv (Folha v) = 1
contaArv (No v a1 a2) = 1 + contaArv a1 + contaArv a2

maiorElem :: Arvore Int -> Int
maiorElem (Folha v) = v
maiorElem (No v a1 a2) = max v (max (maiorElem a1) (maiorElem a2))

contaVezes :: Int -> Arvore Int -> Int
contaVezes x (Folha v)
 |x == v = 1
 |otherwise = 0
contaVezes x (No v a1 a2)
 |v == x = 1 + contaVezes x a1 + contaVezes x a2
 |otherwise = 0 + contaVezes x a1 + contaVezes x a2 
 
refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha v) = Folha v
refleteArvore (No v a1 a2) = No v (refleteArvore a2) (refleteArvore a1)

alturaArvore :: Arvore a -> Int
alturaArvore (Folha v) = 1
alturaArvore (No v a1 a2) = 1 + max (alturaArvore a1) (alturaArvore a2)

arvoreLista :: Arvore a -> [a]
arvoreLista (Folha v) = [v]
arvoreLista (No v a1 a2) = v : arvoreLista a1 ++ arvoreLista a2

mapTree :: (a -> b) -> Arvore a -> Arvore b
mapTree f (Folha v) = (Folha (f v))
mapTree f (No v a1 a2) = No (f v) (mapTree f a1) (mapTree f a2)

