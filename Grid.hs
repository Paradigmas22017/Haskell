module Grid(initialGrid, printArray, newArray, repeatNTimes,
	repeatNTimesWithoutSpaces, arrFinal, takeOneElement, setValue, printNewMatrix, findBomb, makeExplosion) where

import Data.Array
import Data.List.Split
import Control.Monad
import System.IO
import Text.Printf (printf)

gridWidth :: Int
gridWidth = 10

gridHeight :: Int
gridHeight = 10


initialGrid :: Array (Int, Int) Int
initialGrid =
	array ((0,0),(gridWidth,gridHeight)) [((x,y), generator x y) | x<-[0..gridWidth], y<-[0..gridHeight]]

generator :: Int -> Int -> Int
generator x y
	| x == 0 || y == 0 || x == gridWidth || y == gridHeight = 9
	| x == 1 && y == 1 = 5
	| x == 1 && y == 2 = 0
	| x == 1 && y == 3 = 0
	| x == 2 && y == 2 = 0
	| (mod (x*y) 4 == 1 || mod (x*y) 4 == 3) = 9
	| (mod (x*y) 7 == 1 || mod (x*y) 7 == 3 || mod (x*y) 7 == 4 || mod (x*y) 7 == 6 || mod (x*y) 7 == 3) = 1
	| otherwise = 0

printArray arr =
  unlines [unwords [show (arr ! (x, y)) | x <- [0..gridWidth]] | y <- [0..gridHeight]]

--newArray :: Int -> Int -> Array -> Array
newArray i j arr = setValue (i, j) 5 arr

--moveD arr posI posY = gameLoop ((setValue posI+1 posY arr) (posI+1) posY)
printNewMatrix :: Int -> Int -> Int -> Int -> Int -> Int -> Array(Int, Int) Int -> IO()
printNewMatrix player_x player_y bomb_x bomb_y player_value bomb_value arr =
	if bomb_x /= (-1) && bomb_y /= (-1)
		then repeatNTimes (printArray (setValue (player_x, player_y) bomb_value arr)) (-1)
		else if bomb_x == (-1) && bomb_y == (-1)
			then repeatNTimes (printArray (setValue (player_x, player_y) player_value arr)) (-1)
			else print "Caso Errado"

--49 é obtido fazendo-se repeatNTimes length arrFinal no terminal
--repeatNTimes :: Array (Int, Int) Int -> Int
repeatNTimes arr (-1) = do
	putChar '\n'
	repeatNTimes arr (0)
repeatNTimes arr 242 = putChar '\n'
repeatNTimes arr 241 = do
	putChar((arr)!!241)
	repeatNTimes arr (242)
repeatNTimes arr n = do
  	putChar((arr)!!n)
  	repeatNTimes arr (n+1)

arrFinal :: Array (Int, Int) Int -> String
arrFinal arr = printArray arr

repeatNTimesWithoutSpaces :: Array (Int, Int) Int -> Int -> IO()
repeatNTimesWithoutSpaces arr 49 = putChar((arrFinal arr)!!49)
repeatNTimesWithoutSpaces arr n = do
  if ((arrFinal arr)!!n) /= ' '
    then do putChar((arrFinal arr)!!n)
            repeatNTimesWithoutSpaces arr (n+1)
    else do repeatNTimesWithoutSpaces arr (n+1)

takeOneElement :: Int -> Int -> Array (Int, Int) Int -> Int
takeOneElement i j arr =  arr!(i,j)

setValue :: (Int, Int) -> Int -> Array (Int, Int) Int-> Array (Int, Int) Int
setValue (x, y) value ar = ar // [((x,y), value)]



findBomb :: Int -> Int -> Array (Int, Int) Int -> [Int]
findBomb i j arr
	| (arr ! (i, j)) == 7 = [i, j]
	| (arr ! (i, j) /= 7 && i < gridWidth) = findBomb (i+1) j arr
	| (arr ! (i, j	) /= 7 && i == gridWidth && j < gridHeight) = findBomb 0 (j+1) arr
	| (arr ! (i, j	) /= 7 && i == gridWidth && j == gridHeight) = [-1, -1]
	| otherwise = [-10, -10] -- Caso de erro



makeExplosion :: Int -> Int -> [Int] -> Int -> Int -> Array (Int, Int) Int -> Array (Int, Int) Int
makeExplosion player_x player_y bomb_position player bomb arr =
	setValue (bomb_position!!0, bomb_position!!1) 0
		(isExplosible [bomb_position!!0+1, bomb_position!!1]
			(isExplosible [bomb_position!!0-1, bomb_position!!1]
				(isExplosible [bomb_position!!0, bomb_position!!1+1]
					(isExplosible [bomb_position!!0, bomb_position!!1-1] arr))))


isExplosible :: [Int] -> Array (Int, Int) Int-> Array (Int, Int) Int
isExplosible bomb_position arr
	| (arr ! (bomb_position!!0, bomb_position!!1)) /= 9 = setValue (bomb_position!!0, bomb_position!!1) 0 arr
	| otherwise = arr
