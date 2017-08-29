import Data.Array
import Data.List.Split
import Control.Monad

mat = array ((0,0),(4,4)) [((1,1),0), ((1,2),0), ((1,3),0), ((2,1),1), ((2,2),1), ((2,3), 0), ((3,1), 1), ((3,2), 0), ((3,3), 0), ((0, 0), 9), ((0, 1), 9), ((0, 2), 9), ((0, 3), 9), ((0, 4), 9), ((1, 0), 9), ((2, 0), 9), ((3, 0), 9), ((4, 0), 9), ((4, 1), 9), ((4, 2), 9), ((4, 3), 9), ((4, 4), 9), ((1, 4), 9), ((2, 4), 9), ((3, 4), 9), ((4, 4), 9)]

printArray arr =
  unlines [unwords [show (arr ! (x, y)) | x <- [0..4]] | y <- [0..4]]

arrFinal :: String
arrFinal = printArray mat

--49 é obtido fazendo-se repeatNTimes length arrFinal no terminal
repeatNTimes :: Int -> IO()
repeatNTimes 49 = putChar((arrFinal)!!49)
repeatNTimes n =
 do
  putChar((arrFinal)!!n)
  repeatNTimes (n+1)

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

-- setValue :: (Int, Int) -> Int -> Array -> Array
setValue (x,y) a ar = ar // [((x,y), a)]

-- replaceOneElement i j value =
    
--main = repeatNTimes length(arrFinal)
main = repeatNTimes 0
