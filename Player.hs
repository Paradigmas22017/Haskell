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
player :: Char
player = '@'

bomb :: Char
bomb = 'B'

moveRight :: Int -> Int -> Array (Int, Int) Char -> IO()
moveRight i j arr
	| (takeOneElement i j arr) == player = case (takeOneElement (i+1) j arr) of
		' ' -> gameLoop (i+1) j (-1) (-1) player bomb (setValue ((i+1), j) player (setValue ((i), j) ' ' arr));
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;
	| (takeOneElement i j arr) == bomb = case (takeOneElement (i+1) j arr) of
		' ' -> gameLoop (i+1) j (-1) (-1) player bomb (setValue ((i+1), j) player arr); -- Sem resetar para zero
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;

moveLeft :: Int -> Int -> Array (Int, Int) Char -> IO()
moveLeft i j arr
 	| (takeOneElement i j arr) == player = case takeOneElement (i-1) j arr of
		' ' -> gameLoop (i-1) j (-1) (-1) player bomb (setValue ((i-1), j) player (setValue ((i), j) ' ' arr));
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;
	| (takeOneElement i j arr) == bomb = case takeOneElement (i-1) j arr of
		' ' -> gameLoop (i-1) j (-1) (-1) player bomb (setValue ((i-1), j) player arr);
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;

moveUp :: Int -> Int -> Array (Int, Int) Char -> IO()
moveUp i j arr
	| (takeOneElement i j arr) == player = case takeOneElement i (j-1) arr of
		' ' -> gameLoop i (j-1) (-1) (-1) player bomb (setValue (i, (j-1)) player (setValue ((i), j) ' ' arr));
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;
	| (takeOneElement i j arr) == bomb = case takeOneElement i (j-1) arr of
		' ' -> gameLoop i (j-1) (-1) (-1) player bomb (setValue (i, (j-1)) player arr);
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;

moveDown :: Int -> Int -> Array (Int, Int) Char -> IO()
moveDown i j arr
	| (takeOneElement i j arr) == player = case takeOneElement i (j+1) arr of
		' ' -> gameLoop i (j+1) (-1) (-1) player bomb (setValue (i, (j+1)) player (setValue ((i), j) ' ' arr));
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;
	| (takeOneElement i j arr) == bomb = case takeOneElement i (j+1) arr of
		' ' -> gameLoop i (j+1) (-1) (-1) player bomb (setValue (i, (j+1)) player arr);
		'O' -> gameLoop i j (-1) (-1) player bomb arr;
		'#' -> gameLoop i j (-1) (-1) player bomb arr;
		bomb -> gameLoop i j (-1) (-1) player bomb arr;


-- BOMB RELATED
putBomb :: Int -> Int -> Array (Int, Int) Char -> IO()
putBomb i j arr = case takeOneElement i j arr of
	player -> gameLoop i j i j player bomb (setValue ((i), j) bomb arr);

hasBomb :: Int -> Int -> [Int] -> Array (Int, Int) Char -> IO()
hasBomb player_x player_y bomb_position arr
	| bomb_position == [-1, -1] = gameLoop player_x player_y (-1) (-1) player bomb arr
	| bomb_position == [-10, -10] = putStrLn "\n\n\n\n\tErro grave!\n\n\n\n\n"
	| otherwise = checkPlayer player_x player_y bomb_position player bomb (makeExplosion  player_x player_y bomb_position player bomb arr)


checkPlayer :: Int -> Int -> [Int] -> Char -> Char -> Array (Int, Int) Char -> IO()
checkPlayer player_x player_y bomb_position player bomb arr
	| findElement 0 0 player arr == [-1, -1] = finishGame arr
	| otherwise = gameLoop player_x player_y (-1) (-1) player bomb arr


-- player_x player_y bomb_x bomb_y player_value bomb_value arr
gameLoop :: Int -> Int -> Int -> Int -> Char -> Char -> Array (Int, Int) Char -> IO()
-- element deve ser player ou bomb
gameLoop player_x player_y bomb_x bomb_y player_value bomb_value arr = do {
		printNewMatrix player_x player_y bomb_x bomb_y player_value bomb_value arr;
        opcao <- getChar;
        actionConditions opcao player_x player_y bomb_x bomb_y player_value bomb_value arr
	}

actionConditions :: Char -> Int -> Int -> Int -> Int -> Char-> Char -> Array (Int, Int) Char -> IO()
actionConditions opcao player_x player_y bomb_x bomb_y player bomb arr
	| opcao == 'd' = moveRight player_x player_y arr
	| opcao == 'a' = moveLeft player_x player_y arr
	| opcao == 'w' = moveUp player_x player_y arr
	| opcao == 's' = moveDown player_x player_y arr
	| opcao == 'c' = putBomb player_x player_y arr
	| opcao == ' ' = hasBomb player_x player_y (findElement 0 0 bomb arr) arr
	| otherwise = gameLoop player_x player_y bomb_x bomb_y player bomb arr
