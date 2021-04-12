import System.IO
import System.Process
import Data.List
import Data.Function


type Jogadores = [Jogador]
type Nome = String
type Vez = Int
type Tabela = [Char]
data Jogador = Jogador Nome
 deriving (Show, Read)

jogar :: IO ()
jogar = do
 opcoes []
 return ()

opcoes :: Jogadores -> IO Jogadores
opcoes dados = do
 system "cls" -- BUGADO PARA LINUX
 putStrLn "                         Trabalho Haskell - Jogo da Velha"
 putStrLn "Digite 1 para jogar"
 putStrLn "Digite 0 para sair"
 putStr "Opção: "
 op <- getChar
 getChar
 executarOpcaoEscolhida dados op

executarOpcaoEscolhida :: Jogadores -> Char -> IO Jogadores
executarOpcaoEscolhida dados '1' = prepararJogo dados
executarOpcaoEscolhida dados '0' = do
 putStrLn ("\nExit\n")
 return dados
executarOpcaoEscolhida dados _ = do
 putStrLn ("\nDigite uma opcao valida ")
 opcoes dados

prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
 putStrLn ("\n")
 putStr ("Jogador 1: ")
 jog1 <- getLine  
 putStr ("Jogador 2: ")
 jog2 <- getLine  
 inicioDeJogo dados jog1 jog2

inicioDeJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
inicioDeJogo dados jog1 jog2 = do
 system "cls"
 executarJogo dados ['1', '2', '3', '4', '5', '6', '7', '8', '9'] jog1 jog2 0

executarJogo :: Jogadores -> Tabela -> Nome -> Nome -> Vez -> IO Jogadores
executarJogo dados tabela jog1 jog2 vez = do
 putStrLn ("\n" ++ "                           *****  " ++ jog1 ++ " vs " ++ jog2 ++ "  *****")
 putStrLn ("\n " ++ jog1 ++ " será o X e " ++ jog2 ++ " será o O .")
 putStrLn ("\n" ++ "                              " ++ (show (tabela !! 0)) ++ " | " ++ (show (tabela !! 1)) ++ " | " ++ (show (tabela !! 2)) ++
  "\n                              ---------------\n" ++ "                              " ++
  (show (tabela !! 3)) ++ " | " ++ (show (tabela !! 4)) ++ " | " ++ (show (tabela !! 5)) ++
  "\n                              ---------------\n" ++ "                              " ++
  (show (tabela !! 6)) ++ " | " ++ (show (tabela !! 7)) ++ " | " ++ (show (tabela !! 8)) ++
  "\n")
 if (verificarVitoriajog1 tabela) then do
  putStrLn ("                  ********** " ++ jog1 ++ " VENCEU O JOGO **********")
  getChar
  opcoes dados
 else do
  if (verificarVitoriajog2 tabela) then do
   putStrLn ("                   ********** " ++ jog2 ++ " VENCEU O JOGO **********")
   getChar
   opcoes dados
    else do
     if ((length (intersect "123456789" tabela)) == 0) then do
      putStrLn ("O jogo empatou")
      getChar
      opcoes dados
      else do
		if (vez == 0) then do
			putStr ("Vez de " ++ jog1 ++ " escolher uma opção: ")
			op <- getChar
			getChar
			system "cls"

			if not (elem op "123456789") then do
				putStrLn "\nDigite uma opcao valida"
				executarJogo dados tabela jog1 jog2 0

			else
				if not (elem op tabela) then do
				putStrLn "\nOpção já esta sendo usada"
				executarJogo dados tabela jog1 jog2 0

				else
						executarJogo dados (modificarTabela tabela vez op) jog1 jog2 1
		else do

			putStr ("Vez de " ++ jog2 ++ " escolher uma opção: ")
			op <- getChar
			getChar
			system "cls"

			if not (elem op "123456789") then do
			putStrLn "\nDigite uma opcao valida"
			executarJogo dados tabela jog1 jog2 1
			else
				if not (elem op tabela) then do
					putStrLn "\nOpção já esta sendo usada"
					executarJogo dados tabela jog1 jog2 1
						
				else
					executarJogo dados (modificarTabela tabela vez op) jog1 jog2 0

verificarVitoriajog1 :: Tabela -> Bool
verificarVitoriajog1 tabela
 | (((tabela !! 0) == 'X') && ((tabela !! 1) == 'X') && ((tabela !! 2) == 'X')) = True
 | (((tabela !! 3) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 5) == 'X')) = True
 | (((tabela !! 6) == 'X') && ((tabela !! 7) == 'X') && ((tabela !! 8) == 'X')) = True
 | (((tabela !! 0) == 'X') && ((tabela !! 3) == 'X') && ((tabela !! 6) == 'X')) = True
 | (((tabela !! 1) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 7) == 'X')) = True
 | (((tabela !! 2) == 'X') && ((tabela !! 5) == 'X') && ((tabela !! 8) == 'X')) = True
 | (((tabela !! 0) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 8) == 'X')) = True
 | (((tabela !! 2) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 6) == 'X')) = True
 | otherwise = False

modificarTabela :: Tabela -> Vez -> Char -> Tabela
modificarTabela (x:xs) vez e
 |((x == e) && (vez == 0)) = (['X'] ++ xs)
 |((x == e) && (vez == 1)) = (['O'] ++ xs)
 |otherwise = x:(modificarTabela xs vez e)

verificarVitoriajog2 :: Tabela -> Bool
verificarVitoriajog2 tabela
 | (((tabela !! 0) == 'O') && ((tabela !! 1) == 'O') && ((tabela !! 2) == 'O')) = True
 | (((tabela !! 3) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 5) == 'O')) = True
 | (((tabela !! 6) == 'O') && ((tabela !! 7) == 'O') && ((tabela !! 8) == 'O')) = True
 | (((tabela !! 0) == 'O') && ((tabela !! 3) == 'O') && ((tabela !! 6) == 'O')) = True
 | (((tabela !! 1) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 7) == 'O')) = True
 | (((tabela !! 2) == 'O') && ((tabela !! 5) == 'O') && ((tabela !! 8) == 'O')) = True
 | (((tabela !! 0) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 8) == 'O')) = True
 | (((tabela !! 2) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 6) == 'O')) = True
 | otherwise = False

