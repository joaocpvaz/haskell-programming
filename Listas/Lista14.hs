-- :t elemento - saber tipo mais geral
-- classe Eq é por causa do '==', não pode comparar funções
elemento n [] = False
elemento n (x:xs) = n == x || elemento n xs

data SocioClube = Socio Int String String

eqSocio :: SocioClube -> SocioClube -> Bool
eqSocio (Socio c1 _ _) (Socio c2 _ _) = c1 == c2

instance Eq SocioClube where -- Se fosse deriving ele compararia tudo, retornando falso 
 (==) = eqSocio

--class Show a where DEFINIÇAO JA EXISTENTE
-- show :: a -> String

showSocio :: SocioClube -> String
showSocio (Socio i s1 s2) = " ( " ++ " Codigo: " ++ show i ++ ", Nome: " ++ show s1 ++ ", Fone: " ++ s2 ++ " )"

instance Show SocioClube where
 show = showSocio
-----------------------------------------------
data TUnica a = T1 a a 

showTUnica :: Show a => TUnica a -> String
showTUnica (T1 a b) = "(" ++ show a ++ "," ++ show b ++ ")"

instance Show a => Show (TUnica a) where
 show = showTUnica
-----------------------------------------------
data Tupla a b = T2 a b

showTupla :: (Show a, Show b) => Tupla a b -> String
showTupla (T2 a b) = "(" ++ show a ++ "," ++ show b ++ ")"

instance (Show a, Show b) => Show (Tupla a b) where
 show = showTupla
 -- ==================================================================================

data Lista a = Cons a (Lista a) | Vazio

printLista :: (Show a) => Lista a -> String
printLista Vazio = ""
printLista l@(Cons a b) = "[" ++ show a ++ printListaAux b ++ "]"

printListaAux :: (Show a) => Lista a -> String
printListaAux Vazio = []
printListaAux (Cons a b) = "," ++ show a ++ printListaAux b 

instance Show a => Show (Lista a) where
 show = printLista



----------------------------------------------

data Tree a = Nodo a (Tree a) (Tree a) | Folha a -- para usar não chamar a printTree, apenas colocar a árvore no ghci!

printTree ::(Show a) => Tree a -> String
printTree (Folha a) = "[" ++ show a ++ "]"
printTree (Nodo a b c) = "[" ++ show a  ++ printTreeAux b ++ printTreeAux c ++ "]"

printTreeAux ::(Show a) => Tree a -> String
printTreeAux (Folha a) = "," ++ show a 
printTreeAux (Nodo a b c) = "," ++ show a ++ printTreeAux b ++ printTreeAux c

instance Show a => Show (Tree a) where
 show = printTree


-- ==================================================================================

data Arvore a b = NodoA a (Arvore a b) (Arvore a b) | NodoB b (Arvore a b) (Arvore a b) | FolhaA a | FolhaB b

printArvore :: (Show a, Show b) => Arvore a b -> String
printArvore l@(NodoA a b c) = "(" ++ "[" ++ show a ++ printAs b ++ printAs c ++ "]" ++ "," ++ "[" ++ printBs b ++ printBs c ++ "]" ++ ")"
printArvore l@(NodoB a b c) = "(" ++ "[" ++ printAs b ++ printAs c ++ "]" ++ "," ++ "[" ++ show a ++ printBs b ++ printBs c ++ "]" ++ ")"

printAs :: (Show a, Show b) => Arvore a b -> String
printAs (FolhaB a) = ""
printAs (FolhaA a) = "," ++ show a 
printAs (NodoB a b c) = printAs b ++ printAs c
printAs (NodoA a b c) = "," ++ show a ++ printAs b ++ printAs c

printBs :: (Show a, Show b) => Arvore a b -> String
printBs (FolhaA a) = ""
printBs (FolhaB a) = "," ++ show a
printBs (NodoA a b c) = printBs b ++ printBs c
printBs (NodoB a b c) = "," ++ show a ++ printBs b ++ printBs c

instance (Show a, Show b) => Show (Arvore a b) where
 show = printArvore

-- ==================================================================================
instance Ord SocioClube where
 (Socio a _ _) `compare` (Socio d _ _) = a `compare` d

iSort :: [SocioClube]->[SocioClube]
iSort [] = []
iSort (x:xs) = ins x(iSort xs)

ins :: SocioClube -> [SocioClube] -> [SocioClube]
ins a [] = [a]
ins a(x:xs)
 | a <= x = a : x : xs
 | otherwise = x : ins a xs





