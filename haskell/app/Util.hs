module Util where 
import Dados
import Text.Read (readMaybe)
import System.Directory
import System.IO
import System.Process
import Data.List.Split (splitOn)
import Grafico
--Função para calcular o total de horas baseado na lista de Demandas
totalHours :: [Demand] -> Int
totalHours demands = round $ sum (map (\demand -> 1.5 * fromIntegral (complexity demand)) demands)

--Quebra as horas nos dias da semana
splitHours :: Int -> [[Int]]
splitHours n
  | n <= 0 = replicate 5 [0]
  | n >= 40 = replicate 5 [8]
  | otherwise = [min 8 n] : splitHours (n - min 8 n)


--Função que retorna a lista de tempos baseado nas demandas
predictTime :: [Demand]-> [[Int]]
predictTime a = do
  let total = totalHours a
  take 5 (splitHours total)


--Função para exibir o total 
showTotal :: IO()
showTotal = do
  csv <-readCSV ()
  let lista = quebraLinha csv
  let total = totalHours lista
  let tempo_dias = predictTime lista
  createGraph tempo_dias
  putStrLn "O gráfico foi gerado com sucesso!"
  putStrLn "A ordem de execução das tarefas é:"
  printDemands lista
  addFinal lista
  putStrLn( "O total de horas estimadas é:" ++ (show total))
  putStrLn "Aperte uma tecla para voltar ao menu"
  temp <- getLine
  putStrLn temp

--Função para escrever o header no csv
writeHeader ::  String -> IO ()
writeHeader file = do
    exists <- doesFileExist file
    if exists
        then
            putStrLn "Arquivo base já criado"
        else
            writeFile file (cabecalho ++ "\n")

--Função para zerar as Demandas
cleanDemands :: IO()
cleanDemands = do
  writeFile banco (cabecalho ++ "\n")
  putStrLn "Demandas limpas com sucesso!"
  putStrLn "Aperte uma tecla para voltar ao menu"
  temp <- getLine
  putStrLn temp


--Função para exibir o menu inicial
showMenu :: IO ()
showMenu = do
  putStrLn "Menu:"
  putStrLn "1. Nova Demanda"
  putStrLn "2. Lista Demandas"
  putStrLn "3. Gráfico Semanal"
  putStrLn "4. Zerar Demandas"
  putStrLn "5. Sair"
  
--Função para ler a escolha do usuário
readChoice :: IO Int
readChoice = do
  putStrLn "Entre com sua escolha (1-5):"
  choice <- getLine
  case readMaybe choice of
    Just n -> return n
    Nothing -> do
      putStrLn "Escolha inválida. Entre um número entre 1 e 5."
      readChoice

--Função para ler um inteiro entre um e cinco
readOneFive :: String-> IO Int
readOneFive a = do
  putStrLn a
  choice <- getLine
  case readMaybe choice of
    Just n | n >= 1 && n <= 5 -> return n
    _ -> do
      putStrLn "Entrada inválida. Tente novamente."
      readOneFive a

--Função para ler uma String 
readName :: String -> IO String
readName a = do
  putStrLn a
  getLine
  
--Função interface para adicionar uma nova demanda no csv
addDemand :: IO ()
addDemand = do
  name <- readName "Entre com o nome"
  complexity <- readOneFive "Entre com a complexidade"
  priority <- readOneFive "Entre com a prioridade"
  let demand = Demand name complexity priority
  addCSV (show demand) banco
--Função para ler as demandas do csv
readCSV :: () -> IO [String]
readCSV () = lines <$> readFile banco

--Função interface para exibir as demandas do csv
readDemands:: IO()
readDemands= do
  csv <-readCSV ()
  let lista = quebraLinha csv
  print (lista)
  putStrLn "Aperte uma tecla para voltar ao menu"
  temp <- getLine
  putStrLn temp

--Função para quebrar as linhas em demandas
quebraLinha :: [String] -> [Demand]
quebraLinha (header:lines) = map createDemand lines
  where
    createDemand line = case splitOn "," line of
      [name, complexity, priority] -> Demand name (read complexity) (read priority)
      _ -> error "Erro ao ler o csv"

--Função para adicionar a demanda no csv
addCSV :: String -> String -> IO()
addCSV demand file = do
    writeHeader file
    withFile  file AppendMode $ \handle -> do
        hPutStr handle demand
    putStrLn "Demanda adicionada"
--Função para jogar os dados finais para outro csv
addFinal :: [Demand] -> IO()
addFinal demands = do
  writeFile dadosFinais (cabecalhoFinal ++ "\n")
  withFile dadosFinais AppendMode $ \handle -> do
    hPutStr handle (finalDemands demands)
--Função para escrever uma linha em um arquivo
writeLine ::  String -> String -> IO ()
writeLine line file = do
    exists <- doesFileExist file
    if exists
        then
            putStrLn "Arquivo existe"
        else
            writeFile file (line ++ "\n")

--Função para adicionar uma linha em um arquivo
addLine :: String -> String -> IO ()
addLine line file = do
    withFile  file AppendMode $ \handle -> do
        hPutStrLn handle line

--Função que encaminha as ações
acoes:: Int-> IO()
acoes a = do
  if a == 1 then
    addDemand
  else if a == 2 then
    readDemands 
  else if a == 3 then
    showTotal
  else if a == 4 then
    cleanDemands
  else
    readDemands

--Função para limpar o terminal 
clearScreen :: IO ()
clearScreen = do
  _ <- system "clear"  -- No windows precisa ser  "cls" no lugar do clear
  return ()
