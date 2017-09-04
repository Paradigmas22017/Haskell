import Control.Concurrent

obter_fatores :: Int -> [Int]
obter_fatores num = [ x | x <- [1 .. num-1], ((mod num x) == 0)]

eh_perfeito :: Int -> Bool
eh_perfeito num
	| ((sum (obter_fatores num)) == num) = True
	| otherwise = False

obter_perfeitos :: Int -> [Int]
obter_perfeitos n = [x | x <- [1 .. n], ((eh_perfeito x) == True)]


main = do

	forkIO $ do
			putStrLn("Obtendo todos os números perfeitos até 10000...")
			putStrLn("Números perfeitos: " ++ (show (obter_perfeitos 10000)))
			putStrLn("Acabou de obter os números perfeitos!")

	forkIO $ do
	-- Delay de 3 segundos
			threadDelay 3000000
			putStrLn("\n Executando outra thread")

	-- Delay de 40 segundos
	threadDelay 40000000
	putStrLn("Fim!")
