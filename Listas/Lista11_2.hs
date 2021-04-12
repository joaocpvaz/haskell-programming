data Arvore a = No a [Arvore a] | Folha
 deriving(Eq,Show)


multDois :: Arvore Int -> Arvore Int
multDois Folha = Folha
multDois (No a l) = No (a*2) (map multDois l)

qtdElementos :: Arvore Int -> Int
qtdElementos Folha = 0
qtdElementos (No a l) = 1 + (foldr1 qtdElementos l)


