{-# LANGUAGE OverloadedStrings #-}
module Dados where

import Data.Csv
import Data.List (sortBy,intercalate)

--Demanda
data Demand = Demand
  { name :: String
  , complexity :: Int
  , priority :: Int
  }
--Instância personalizada para exibir uma demanda
instance Show Demand where
  show (Demand n c p) =
    n ++ ", " ++ show c ++ ", " ++ show p ++"\n"

--Instância para aceitar strings nos Ints
instance FromNamedRecord Demand where
  parseNamedRecord m = do
    n <- m .: "Nome"
    c <- m .: "Complexidade" >>= parseComplexity
    p <- m .: "Prioridade" >>= parsePriority
    return $ Demand n c p
    where
      parseComplexity :: Read a => String -> Parser a
      parseComplexity str =
        case reads str of
          [(val, "")] -> return val
          _ -> fail "Valor inválido de complexidade"
      
      parsePriority :: Read a => String -> Parser a
      parsePriority str =
        case reads str of
          [(val, "")] -> return val
          _ -> fail "Valor inválido de prioridade"

--Função para criar uma demanda baseada em uma lista de strings
createDemand :: [String] -> Demand
createDemand [n, complexityStr, priorityStr] =
  Demand n (read complexityStr) (read priorityStr)
createDemand _ = error "Erro ao tentar criar demanda"
--Função para imprimir a lista de demandas ordenadas
printDemands :: [Demand] -> IO ()
printDemands demands = do
  let sortedDemands = sortBy (\d1 d2 -> compare (priority d2) (priority d1)) demands
  mapM_ printDemand sortedDemands
--Função para imprimir uma demanda com horas estimadas
printDemand :: Demand -> IO ()
printDemand demand = putStrLn $ name demand ++ ", Horas estimadas: " ++ show (fromIntegral(complexity demand) * 1.5::Double) ++ ", Prioridade: " ++ show (priority demand)

--Função para retornar uma String única da lista de demandas
finalDemands :: [Demand] -> String
finalDemands demands = intercalate "\n" (map finalDemand (sortBy (\d1 d2 -> compare (priority d2) (priority d1)) demands))
--Função para retornar a String de cada demanda no formato com horas
finalDemand :: Demand -> String
finalDemand demand = name demand ++ ","++ show (fromIntegral(complexity demand) * 1.5::Double) ++ "," ++ show (priority demand)
--Variáveis que são usadas em outros módulos
cabecalho::String
cabecalho = "Nome,Complexidade,Prioridade"
cabecalhoFinal::String
cabecalhoFinal = "Nome,Horas,Prioridade"
dadosFinais::String
dadosFinais = "dadosFinais.csv"
banco::String
banco = "demandas.csv"
dias:: [String]
dias = ["Segunda","Terça","Quarta","Quinta","Sexta"]
titles :: [String]
titles = ["Tarefas"]
