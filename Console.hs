import System.Console.ANSI --biblioteca que move o cursor 
import Data.Char
import System.IO

-- >cabal update
-- >cabal install ansi-terminal
-- sdl2


main                    :: IO ()
main =  do hSetEcho stdin False --desabilita ficar printando o caracter
           hideCursor -- tira cursor
           clearScreen --limpa tela
           hPutChar stdout '*'
           move

move :: IO ()
move = do  c <- hGetChar stdin
           case (ord c) of --codigo asquii
             114 -> moveUp -- numero dos simbolos r d f c
             99  -> moveDown
             100 -> moveLeft
             102 -> moveRight
             _  -> return ()



moveUp :: IO ()
moveUp = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorUp stdout 1
       hPutChar stdout '*'
       move

moveDown :: IO ()
moveDown = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorDown stdout 1
       hPutChar stdout '*'
       move
moveLeft :: IO ()
moveLeft = do 
       clearScreen
       hCursorBackward stdout 1
       hCursorBackward stdout 1
       hPutChar stdout '*'
       move

moveRight :: IO ()
moveRight = do 
       clearScreen
       hCursorForward stdout 1
       hPutChar stdout '*'
       move
