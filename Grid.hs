module Grid(initialGrid, printArray, newArray, repeatNTimes,
	repeatNTimesWithoutSpaces, arrFinal, takeOneElement, setValue, printNewMatrix) where

import Data.Array
import Data.List.Split
import Control.Monad
import System.IO
import Text.Printf (printf)

initialGrid :: Array (Int, Int) Int
initialGrid = array ((0,0),(4,4)) [((1,1),0), ((1,2),0), ((1,3),0), ((2,1),1), ((2,2),1),
	((2,3), 0), ((3,1), 1), ((3,2), 0), ((3,3), 0), ((0, 0), 9), ((0, 1), 9),
	((0, 2), 9), ((0, 3), 9), ((0, 4), 9), ((1, 0), 9), ((2, 0), 9), ((3, 0), 9),
	((4, 0), 9), ((4, 1), 9), ((4, 2), 9), ((4, 3), 9), ((4, 4), 9), ((1, 4), 9),
	((2, 4), 9), ((3, 4), 9), ((4, 4), 9)]

printArray arr =
  unlines [unwords [show (arr ! (x, y)) | x <- [0..4]] | y <- [0..4]]

--newArray :: Int -> Int -> Array -> Array
newArray i j arr = setValue (i, j) 5 arr

--moveD arr posI posY = gameLoop ((setValue posI+1 posY arr) (posI+1) posY)

printNewMatrix x y value arr = repeatNTimes (printArray (setValue (x, y) value arr)) (-1)

--49 é obtido fazendo-se repeatNTimes length arrFinal no terminal
--repeatNTimes :: Int -> IO()
repeatNTimes arr (-1) = do
	putChar '\n'
	repeatNTimes arr (0)
repeatNTimes arr 50 = putChar '\n'
repeatNTimes arr 49 = do
	putChar((arr)!!49)
	repeatNTimes arr (50)
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