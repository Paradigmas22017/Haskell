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
player :: Int
player = 5

bomb :: Int
bomb = 7


moveRight :: Int -> Int -> Array (Int, Int) Int -> IO()
moveRight i j arr = case takeOneElement (i+1) j arr of
	0 -> gameLoop (i+1) j (-1) (-1) player bomb (setValue ((i+1), j) player (setValue ((i), j) 0 arr));
	1 -> gameLoop i j (-1) (-1) player bomb arr;
	9 -> gameLoop i j (-1) (-1) player bomb arr;

moveLeft :: Int -> Int -> Array (Int, Int) Int -> IO()
moveLeft i j arr = case takeOneElement (i-1) j arr of
	0 -> gameLoop (i-1) j (-1) (-1) player bomb (setValue ((i-1), j) player (setValue ((i), j) 0 arr));
	1 -> gameLoop i j (-1) (-1) player bomb arr;
	9 -> gameLoop i j (-1) (-1) player bomb arr;

moveUp :: Int -> Int -> Array (Int, Int) Int -> IO()
moveUp i j arr = case takeOneElement i (j-1) arr of
	0 -> gameLoop i (j-1) (-1) (-1) player bomb (setValue (i, (j-1)) player (setValue ((i), j) 0 arr));
	1 -> gameLoop i j (-1) (-1) player bomb arr;
	9 -> gameLoop i j (-1) (-1) player bomb arr;

moveDown :: Int -> Int -> Array (Int, Int) Int -> IO()
moveDown i j arr = case takeOneElement i (j+1) arr of
	0 -> gameLoop i (j+1) (-1) (-1) player bomb (setValue (i, (j+1)) player (setValue ((i), j) 0 arr));
	1 -> gameLoop i j (-1) (-1) player bomb arr;
	9 -> gameLoop i j (-1) (-1) player bomb arr;

putBomb :: Int -> Int -> Array (Int, Int) Int -> IO()
putBomb i j arr = case takeOneElement i j arr of
	5 -> gameLoop i j i j player bomb arr;
	1 -> gameLoop i j (-1) (-1) player bomb arr;
	9 -> gameLoop i j (-1) (-1) player bomb arr;

-- player_x player_y bomb_x bomb_y player_value bomb_value arr
gameLoop :: Int -> Int -> Int -> Int -> Int -> Int-> Array (Int, Int) Int -> IO()
-- element deve ser player ou bomb
gameLoop player_x player_y bomb_x bomb_y player_value bomb_value arr = do { printNewMatrix player_x player_y bomb_x bomb_y  player_value bomb_value arr;
        opcao <- getChar;
        actionConditions opcao player_x player_y bomb_x bomb_y player_value bomb_value arr
	}

actionConditions :: Char -> Int -> Int -> Int -> Int -> Int-> Int -> Array (Int, Int) Int -> IO()
actionConditions opcao player_x player_y bomb_x bomb_y player bomb arr
	| opcao == 'd' = moveRight player_x player_y arr
	| opcao == 'a' = moveLeft player_x player_y arr
	| opcao == 'w' = moveUp player_x player_y arr
	| opcao == 's' = moveDown player_x player_y arr
	| opcao == 'c' = putBomb player_x player_y arr
	| otherwise = gameLoop player_x player_y bomb_x bomb_y player bomb arr
