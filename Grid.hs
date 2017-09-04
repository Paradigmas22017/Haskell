module Grid(initialGrid, printArray, takeOneElement, setValue, printNewMatrix,
	findElement, makeExplosion, finishGame) where

import Data.Array
import Data.List.Split
import Control.Monad
import System.IO
import Text.Printf (printf)
import Control.Concurrent (threadDelay);

gridWidth :: Int
gridWidth = 10

gridHeight :: Int
gridHeight = 10

initialGrid :: Array (Int, Int) Char
initialGrid =
	array ((0,0),(gridWidth,gridHeight)) [((x,y), generator x y) | x<-[0..gridWidth], y<-[0..gridHeight]]

generator :: Int -> Int -> Char
generator x y
	| x == 0 || y == 0 || x == gridWidth || y == gridHeight = '#'
	| x == 1 && y == 1 = '@'
	| x == 1 && y == 2 = ' '
	| x == 1 && y == 3 = ' '
	| x == 2 && y == 2 = ' '
	| (mod (x*y) 4 == 1 || mod (x*y) 4 == 3) = '#'
	| (mod (x*y) 7 == 1 || mod (x*y) 7 == 3 || mod (x*y) 7 == 4 || mod (x*y) 7 == 6 || mod (x*y) 7 == 3) = 'O'
	| otherwise = ' '

printArray :: Array (Int, Int) Char -> String
printArray arr =
	unlines [unwords [show (arr ! (x, y)) | x <- [0..gridWidth]] | y <- [0..gridHeight]]

printNewMatrix :: Int -> Int -> Int -> Int -> Char -> Char -> Array (Int, Int) Char -> IO()
printNewMatrix player_x player_y bomb_x bomb_y player_value bomb_value arr =
	if bomb_x /= (-1) && bomb_y /= (-1)
		then putStr ("\ESC[2J\n" ++ (printArray (setValue (player_x, player_y) bomb_value arr)))
		else if bomb_x == (-1) && bomb_y == (-1)
			then putStr ("\ESC[2J\n" ++ (printArray (setValue (player_x, player_y) player_value arr)))
			else print "Caso Errado"

takeOneElement :: Int -> Int -> Array (Int, Int) Char -> Char
takeOneElement i j arr =  arr!(i,j)

setValue :: (Int, Int) -> Char -> Array (Int, Int) Char-> Array (Int, Int) Char
setValue (x, y) value ar = ar // [((x,y), value)]

findElement :: Int -> Int -> Char -> Array (Int, Int) Char -> [Int]
findElement i j element arr
	| (arr ! (i, j)) == element = [i, j]
	| (arr ! (i, j) /= element && i < gridWidth) = findElement (i+1) j element arr
	| (arr ! (i, j	) /= element && i == gridWidth && j < gridHeight) = findElement 0 (j+1) element arr
	| (arr ! (i, j	) /= element && i == gridWidth && j == gridHeight) = [-1, -1]
	| otherwise = [-10, -10] -- Caso de erro

makeExplosion :: Int -> Int -> [Int] -> Char -> Char -> Array (Int, Int) Char -> Array (Int, Int) Char
makeExplosion player_x player_y bomb_position player bomb arr =
	setValue (bomb_position!!0, bomb_position!!1) ' '
		(isExplosible [bomb_position!!0+1, bomb_position!!1]
			(isExplosible [bomb_position!!0-1, bomb_position!!1]
				(isExplosible [bomb_position!!0, bomb_position!!1+1]
					(isExplosible [bomb_position!!0, bomb_position!!1-1] arr))))

isExplosible :: [Int] -> Array (Int, Int) Char-> Array (Int, Int) Char
isExplosible bomb_position arr
	| (arr ! (bomb_position!!0, bomb_position!!1)) /= '#' = setValue (bomb_position!!0, bomb_position!!1) ' ' arr
	| otherwise = arr

finishGame :: Array (Int, Int) Char -> IO()
finishGame arr = do {
	clear;
	putStrLn "\t\t\t\tThe end!\n\n\n\n\n\n\n\n\n\n\n\n\n";
}

clear :: IO()
clear = putStr "\ESC[2J\n"
