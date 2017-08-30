import Data.Array
import Data.List.Split
import Control.Monad
import System.IO
import Text.Printf (printf)

mat = array ((0,0),(4,4)) [((1,1),0), ((1,2),0), ((1,3),0), ((2,1),1), ((2,2),1),
	((2,3), 0), ((3,1), 1), ((3,2), 0), ((3,3), 0), ((0, 0), 9), ((0, 1), 9),
	((0, 2), 9), ((0, 3), 9), ((0, 4), 9), ((1, 0), 9), ((2, 0), 9), ((3, 0), 9),
	((4, 0), 9), ((4, 1), 9), ((4, 2), 9), ((4, 3), 9), ((4, 4), 9), ((1, 4), 9),
	((2, 4), 9), ((3, 4), 9), ((4, 4), 9)]

printArray arr =
  unlines [unwords [show (arr ! (x, y)) | x <- [0..4]] | y <- [0..4]]

arrFinal :: String
arrFinal = printArray mat

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

repeatNTimesWithoutSpaces :: Int -> IO()
repeatNTimesWithoutSpaces 49 = putChar((arrFinal)!!49)
repeatNTimesWithoutSpaces n = do
  if (arrFinal!!n) /= ' '
    then do putChar((arrFinal)!!n)
            repeatNTimesWithoutSpaces (n+1)
    else do repeatNTimesWithoutSpaces (n+1)

takeOneElement :: Int -> Int -> Int
takeOneElement i j =
    mat!(i,j)

setValue :: (Int, Int) -> Int -> Array (Int, Int) Int-> Array (Int, Int) Int
setValue (x, y) value ar = ar // [((x,y), value)]

-- replaceOneElement i j value =
    
--main = repeatNTimes length(arrFinal)
main = repeatNTimes (printArray (mat)) 0

playerI :: Int
playerI = 1
playerJ :: Int
playerJ = 1

--gameLoop :: IO()
--gameLoop = do {	putStrLn "Menu Principal do Jogo: ";
	--printNewMatrix playerI playerJ 5 mat
--	key <- getLine
--	case key of "d" -> putStrLn "Menu Principal do Jogo: ";
--		'q' -> putStrLn "O jogo vai acabar..."
	--if (key == 'd')
	--	then do gameLoop (setValue posI+1 posY 5 arr) (posI+1) posY
	--	else do Nothing
--	}

gameLoop :: Int -> Int -> Array (Int, Int) Int -> IO()
gameLoop i j arr = do { printNewMatrix i j 5 arr;
        opcao <- getChar;
        case opcao of 'd' -> gameLoop (i+1) j (setValue ((i+1), j) 5 (setValue ((i), j) 0 arr));
			'a' -> gameLoop (i-1) j (setValue ((i-1), j) 5 (setValue ((i), j) 0 arr));
			'w' -> gameLoop i (j-1) (setValue (i, (j-1)) 5 (setValue ((i), j) 0 arr));
			's' -> gameLoop i (j+1) (setValue (i, (j+1)) 5 (setValue ((i), j) 0 arr));
	}

--newArray :: Int -> Int -> Array -> Array
newArray i j arr = setValue (i, j) 5 arr

--moveD arr posI posY = gameLoop ((setValue posI+1 posY arr) (posI+1) posY)

printNewMatrix x y value arr = repeatNTimes (printArray (setValue (x, y) value arr)) (-1)