module Menu (menu) where

import System.IO
import Control.Monad
import Control.Concurrent (threadDelay)

import Player
import Grid

menu :: IO()
menu = do { putStrLn "Menu Principal do Jogo: ";
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
	putStrLn " "; putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";
	putStrLn "Comandos do jogo:";
	putStrLn "W - Andar para cima";
	putStrLn "S - Andar para baixo";
	putStrLn "D - Andar para direita";
	putStrLn "A - Andar para esquerda";
	putStrLn "Barra de espaço - Plantar a bomba";
	putStrLn "C - Explodir a bomba";
	putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";putStrLn " ";
	putStrLn "Carregando o jogo...";
	threadDelay 5000000;
	gameLoop playerI playerJ initialGrid
}
				