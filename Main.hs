module Main(main) where

import Data.Array
import Data.List.Split
import Control.Monad
import System.IO
import Text.Printf (printf)

import Grid
import Player
import Menu

main :: IO()
main = menu