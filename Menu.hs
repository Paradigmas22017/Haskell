module Menu (menu) where

import System.IO
import Control.Monad
import Control.Concurrent (threadDelay)

import Player
import Grid

menu :: IO()
menu = do { clear;
		putStrLn "Menu Principal do Jogo: ";
		putStrLn "1 - Quero escolher o meu personagem";
		putStrLn "2 - Começar a jogar";
		putStrLn "3 - Sair";
		opcao <- getLine;
		case opcao of
			"1" -> putStrLn "Redirecionando para escolha do personagem..."
			"2" -> menuInitGame
			"3" -> putStrLn "Finalizando o jogo..."
}

menuInitGame :: IO()
menuInitGame = do {
	clear;
	putStrLn "Comandos do jogo:";
	putStrLn "W - Andar para cima";
	putStrLn "S - Andar para baixo";
	putStrLn "D - Andar para direita";
	putStrLn "A - Andar para esquerda";
	putStrLn "Barra de espaço - Plantar a bomba";
	putStrLn "C - Explodir a bomba";
	printStringNTimesWithDelay 5 "\n" 0;
	loadingGame;
	gameLoop playerI playerJ initialGrid
}

loadingGame :: IO()
loadingGame = do {
				putStr "Carregando o jogo.";
				threadDelay 500000;
				printStringNTimesWithDelay 8 "." 500000;
				putStrLn ".";
				threadDelay 500000;
}

printStringNTimesWithDelay :: Int -> String -> Int -> IO()
printStringNTimesWithDelay 0 str delay = return ()
printStringNTimesWithDelay n str delay = 
	do
		putStr str;
		threadDelay delay;
		printStringNTimesWithDelay (n-1) str delay

clear :: IO()
clear = putStr "\ESC[2J"