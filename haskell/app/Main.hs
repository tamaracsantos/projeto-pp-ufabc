module Main where

import Control.Monad (unless)
import Util

--Função principal para rodar o menu
main :: IO()
main = do

    showMenu
    choice <- readChoice
    unless (choice == 5) $ do
        putStrLn $ "Você escolheu a opção " ++ show choice
        acoes choice
        clearScreen
        main                                                            
  