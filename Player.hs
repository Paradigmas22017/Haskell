module Player(playerI, playerJ, moveRight, moveLeft, moveUp, moveDown, gameLoop, actionConditions, putBomb) where
import Control.Concurrent
import Data.Array
import Data.List.Split
import Control.Monad
import System.IO
import Text.Printf (printf)

import Grid

playerI :: Int
playerI = 1
playerJ :: Int
playerJ = 1

-- Definindo constante para player e para bomba,
--- serão passadas para o gameloop gerar uma nova matriz
player = 5
bomb = 7

moveRight :: Int -> Int -> Array (Int, Int) Int -> IO()
moveRight i j arr = case takeOneElement (i+1) j arr of
	0 -> gameLoop (i+1) j (setValue ((i+1), j) 5 (setValue ((i), j) 0 arr)) player;
	1 -> gameLoop i j arr player;
	9 -> gameLoop i j arr player;

moveLeft :: Int -> Int -> Array (Int, Int) Int -> IO()
moveLeft i j arr = case takeOneElement (i-1) j arr of
	0 -> gameLoop (i-1) j (setValue ((i-1), j) 5 (setValue ((i), j) 0 arr)) player;
	1 -> gameLoop i j arr player;
	9 -> gameLoop i j arr player;

moveUp :: Int -> Int -> Array (Int, Int) Int -> IO()
moveUp i j arr = case takeOneElement i (j-1) arr of
	0 -> gameLoop i (j-1) (setValue (i, (j-1)) 5 (setValue ((i), j) 0 arr)) player;
	1 -> gameLoop i j arr player;
	9 -> gameLoop i j arr player;

moveDown :: Int -> Int -> Array (Int, Int) Int -> IO()
moveDown i j arr = case takeOneElement i (j+1) arr of
	0 -> gameLoop i (j+1) (setValue (i, (j+1)) 5 (setValue ((i), j) 0 arr)) player;
	1 -> gameLoop i j arr player;
	9 -> gameLoop i j arr player;

putBomb :: Int -> Int -> Array (Int, Int) Int -> IO()
putBomb i j arr = case takeOneElement i j arr of
	5 -> gameLoop i j arr bomb;
	1 -> gameLoop i j arr player;
	9 -> gameLoop i j arr player;

gameLoop :: Int -> Int -> Array (Int, Int) Int -> Int -> IO()
-- element deve ser player ou bomb
gameLoop i j arr element = do { printNewMatrix i j element arr;
        opcao <- getChar;
        actionConditions opcao i j arr element
	}

actionConditions :: Char -> Int -> Int -> Array (Int, Int) Int -> Int -> IO()
actionConditions opcao i j arr element
	| opcao == 'd' = moveRight i j arr
	| opcao == 'a' = moveLeft i j arr
	| opcao == 'w' = moveUp i j arr
	| opcao == 's' = moveDown i j arr
	| opcao == 'c' = putBomb i j arr
	| otherwise = gameLoop i j arr element
