module Player(playerI, playerJ, moveRight, moveLeft, moveUp, moveDown, gameLoop) where

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

moveRight :: Int -> Int -> Array (Int, Int) Int -> IO()
moveRight i j arr = case takeOneElement (i+1) j arr of
	0 -> gameLoop (i+1) j (setValue ((i+1), j) 5 (setValue ((i), j) 0 arr));
	1 -> gameLoop i j arr;
	9 -> gameLoop i j arr;

moveLeft :: Int -> Int -> Array (Int, Int) Int -> IO()
moveLeft i j arr = case takeOneElement (i-1) j arr of
	0 -> gameLoop (i-1) j (setValue ((i-1), j) 5 (setValue ((i), j) 0 arr));
	1 -> gameLoop i j arr;
	9 -> gameLoop i j arr;

moveUp :: Int -> Int -> Array (Int, Int) Int -> IO()
moveUp i j arr = case takeOneElement i (j-1) arr of
	0 -> gameLoop i (j-1) (setValue (i, (j-1)) 5 (setValue ((i), j) 0 arr));
	1 -> gameLoop i j arr;
	9 -> gameLoop i j arr;

moveDown :: Int -> Int -> Array (Int, Int) Int -> IO()
moveDown i j arr = case takeOneElement i (j+1) arr of
	0 -> gameLoop i (j+1) (setValue (i, (j+1)) 5 (setValue ((i), j) 0 arr));
	1 -> gameLoop i j arr;
	9 -> gameLoop i j arr;

gameLoop :: Int -> Int -> Array (Int, Int) Int -> IO()
gameLoop i j arr = do { printNewMatrix i j 5 arr;
        opcao <- getChar;
        case opcao of
        	'd' -> moveRight i j arr;
			'a' -> moveLeft i j arr;
			'w' -> moveUp i j arr;
			's' -> moveDown i j arr;
	}