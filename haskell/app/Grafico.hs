module Grafico where
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Dados
--Função para criar o gráfico tarefas.svg
createGraph:: [[Int]]-> IO()
createGraph temp = do
    toFile def "tarefas.svg" $ do
        layout_title .= "Tarefas realizadas na semana"
        layout_title_style . font_size .= 10
        layout_x_axis . laxis_generate .= autoIndexAxis  dias
        plot $ plotBars <$> bars titles (addIndexes temp)