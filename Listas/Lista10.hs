data Temperatura = Frio | Calor
 deriving(Eq,Show)
--data Boll = True | False
data Estacao = Verao | Outono | Inverno | Primavera -- -> 4 construtores

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Funcionario = Pessoa Nome Idade -- Pessoa é uma tupla que guarde string e inteiros
 deriving(Eq,Show)

type Nome = String
type Idade = Int

andre :: Funcionario
andre = Pessoa "Andre Du Bois" 28

--Pessoa :: Nome -> Idade -> Funcionario

pegaNome :: Funcionario -> Nome
pegaNome (Pessoa nome idade) = nome

pegaIdade :: Funcionario -> Idade
pegaIdade (Pessoa nome idade) = idade

-----

data Forma = Circulo Float |Retangulo Float Float
 deriving(Eq,Show)

-- area(Circulo 15.0) 

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a



data ItemDeLocadora = Cds String String|DVDs String String|Videos String String
 deriving(Eq,Show)

data SocioLocadora = Socio String Int
 deriving(Eq,Show)

data ItensDisponiveis = Itens [ItemDeLocadora]
 deriving(Eq,Show)

data ItemAlugado = ItemAlug SocioLocadora ItemDeLocadora
 deriving(Eq,Show)

data ItensAlugados = ItensAlug [ItemAlugado]
 deriving(Eq,Show)

alugarItem :: ItensDisponiveis -> ItensAlugados -> SocioLocadora -> ItemDeLocadora -> (ItensDisponiveis, ItensAlugados)
alugarItem id (ItensAlug lista) socio item = (retira item id, ItensAlug (ItemAlug socio item :lista))

retornarItensAlugados :: ItensAlugados -> ItensAlugados
retornarItensAlugados (ItensAlug lista) = ItensAlug lista






