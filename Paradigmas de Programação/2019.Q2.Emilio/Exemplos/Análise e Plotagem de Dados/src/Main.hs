-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 10/2019

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T

import Graphics.Vega.VegaLite

-- Função auxiliar para fazer o pipe com o fluxo de dados na direção
-- esquerda para a direita. Apenas inverte a ordem dos parâmetros do
-- pipe tradicional ($).
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)


-- A função length devolve um Int, mas para algulas das medidas
-- estatísticas que estamos interessados pode ser mais interssante um
-- Float, Double, ...
length' :: Num a => [b] -> a
length' = fromIntegral . length

-- Simples representação de uma data. Utiliza a sintaxe de registros
-- para definir implicitamente as funções Dia (construtora) e dia, mes
-- ano (para acesso aos campos do registro)
data Dia = Dia {
  dia :: Int,
  mes :: Int,
  ano :: Int} deriving (Show, Eq)

-- Define uma instância de Ord para um dia. Para ser menor ou igual ao
-- um outro dia ou eles são iguais, ou o ano é menor, ou o ano é igual
-- e ... Note o uso de @ para dar um sinônimo ao padrão que foi casado
instance Ord Dia where
  dia0@(Dia d0 m0 a0) <= dia1@(Dia d1 m1 a1) =
    dia0 == dia1 ||
    a0 < a1      ||
    (a0 == a1 &&
     (m0 < m1  ||
      (m0 == m1 &&
       d0 < d1)))

-- Representa cada uma das medicoes meteorológicas
data Medicao = Medicao {
    idEstacao        :: Int,
    dataMedicao      :: Dia,
    horaMedicao      :: Int,
    temperaturaSeco  :: Maybe Float, -- Algumas entradas estão
    temperaturaUmido :: Maybe Float, -- faltando na base de dados.
    umidadeRelativa  :: Maybe Int,   -- Representamos estes casos como
    pressao          :: Maybe Float, -- Maybe tipo para indicar que
    dirVento         :: Maybe Int,   -- pode ser que não tenhamos uma
    velVento         :: Maybe Float, -- medição naquele caso.
    nebulosidade     :: Maybe Float
    } deriving (Show, Eq)

-- Define uma instância Ord para Medicao baseada na sua data e hora.
instance Ord Medicao where
  m0 <= m1
    | dataMedicao m0 < dataMedicao m1 = True
    | dataMedicao m0 == dataMedicao m1 = horaMedicao m0 <= horaMedicao m1
    | otherwise = False -- Tente reescrever essa função sem utilizar
                        -- guards e utilizando uma expressão booleana
                        -- como aquela da instância Ord para Dia

-- Dada uma representação de uma data "dd/mm/aaaa" devolve um Dia
parseData :: String -> Dia
parseData dt =
    Dia d m a
    where
        [d, m, a] = map read $ splitOn "/" dt

-- Faz a leitura de um possível (Maybe) valor. O ambiente padrão já
-- traz instâncias de Read para os tipos mais comuns como Int e Float
-- que são os que precisamos
readMaybe :: Read a => String -> Maybe a
readMaybe "" = Nothing
readMaybe str = Just $ read str

-- Dada uma linha do arquivo que representa a medição, devolve uma
-- Maybe Medição. Nothing é devolvido caso a linha não esteja no
-- formato esperado.
parseLinha :: [String] -> Maybe Medicao
parseLinha [e0, d0, h0, ts0, tu0, u0, p0, dv0, vv0, n0, _] =
    Just $ Medicao
        (read e0)
        (parseData d0)
        (read h0)
        (readMaybe ts0)
        (readMaybe tu0)
        (readMaybe u0)
        (readMaybe p0)
        (readMaybe dv0)
        (readMaybe vv0)
        (readMaybe n0)
parseLinha _ = Nothing

-- Constante com o padrão do cabeçalho do arquivo usado para fazer seu
-- processamento. Note, inclusive, que o padrão do INMET para o
-- cabeçalho tem um erro em seu último campo.
cabecalho :: String
cabecalho = "Estacao;Data;Hora;TempBulboSeco;TempBulboUmido;UmidadeRelativa;PressaoAtmEstacao;DirecaoVento;VelocidadeVentoNebulosidade;"


-- Dado um arquivo de medidas seguindo o padrão do INMET devolve uma
-- [Medicao] contendo os valores já processados
processaArquivo :: String -> [Maybe Medicao]
processaArquivo file =
       lines file                             -- Quebra o arquivo em linhas
    |> dropWhile (not . isPrefixOf cabecalho) -- Descarta linhas até o cabeçalho
    |> tail                                   -- Descarta o cabeçalho
    |> map (splitOn ";")                      -- Quebra as linhas no ;
    |> map parseLinha                         -- Transforma linha em Medicao



-- Função auxiliar para abrupar uma lista de elementos xs baseando-se
-- no retorno da função f. Note que, como a função se baseia na função
-- groupBy, xs precisa estar ordenada por f caso deseje-se apenas uma
-- entrada por grupo.
groupOn :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn f xs =
  map fin (groupBy sameGroup xs)
  where
    sameGroup a b = f a == f b
    -- Note o uso de ~ no casamento do padrão. O ~ indica que este
    -- padrão é o único padrão (que deveria ser) possível. Assim o
    -- compilador não emite warnings para indicar que não estamos
    -- fazendo o casamento de padrões completo (por exemplo casando
    -- com []). Caso em tempo de execução o padrão não esteja correto
    -- ocorre um erro e a execução pára. Use com muita parcimônia.
    fin ~ys@(y0:_) = (f y0, ys)


-- Dada uma lista de números Real a, calcula a medidas estatísticas
-- correspondentes.
media,desvioPadrao :: (Real a) => [a] -> Double
media xs = realToFrac (sum xs) / length' xs
desvioPadrao xs = 0.25 -- Implementar


-- Dada uma lista de amostras `ms`, calcula a medida estatística
-- `stat` sobre `campo` após agrupar as medidas por `agrupaPor`
calculaEstatPor :: (Real a, Eq b) =>
                   ([a] -> Double)      -- Medida estatística (media, desvioPadrao, ...)
                -> (Medicao -> Maybe a) -- Campo sobre o qual a estatistica deve ser calculada
                -> (Medicao -> b)       -- Campo a ser agrupado
                -> [Medicao]            -- Amostras
                -> [(b, Double)]        -- [(grupo,medida estatistica)]
calculaEstatPor stat campo agrupaPor ms =
  map medidaGeral dadosAgrupados
  where
    dadosAgrupados = groupOn agrupaPor ms
    medidaGeral (g, xs) = (g, stat $ mapMaybe campo xs)

-- Funções auxiliares para o cálculo agrupado por ano. Recebem o campo
-- de interesse e a lista de medições.
calculaMediaPorAno, calculaDesvioPorAno :: Real a => (Medicao -> Maybe a) -> [Medicao] -> [(Int, Double)]
calculaMediaPorAno mediaPor   = calculaEstatPor media mediaPor (ano . dataMedicao)
calculaDesvioPorAno desvioPor = calculaEstatPor desvioPadrao desvioPor (ano . dataMedicao)


-- Faz a plotagem do gráfico dado pelos seguintes parâmetros
plot ::   Double    -- altura
       -> Double    -- largura
       -> [T.Text]  -- Categoria
       -> [Double]  -- Valores eixo x
       -> [Double]  -- Valores eixo y
       -> [Double]  -- Erros
       -> T.Text    -- Titulo
       -> T.Text    -- Nome categorias
       -> T.Text    -- Nome eixo x
       -> T.Text    -- Nome eixo y
       -> T.Text    -- Nome eixo erro
       -> VegaLite  -- plot
plot h w cats dataX dataY dataErr plotTitle catlbl xlbl ylbl errlbl =
  toVegaLite [
    title plotTitle [],
    dat,
    layer (map asSpec [layerRange, layerData]),
    height  h,
    width w
    ]
  where
    -- Define os dados, nomes e tipos
    dat = dataFromColumns []
          . dataColumn catlbl (Strings cats)
          . dataColumn xlbl   (Numbers dataX)
          . dataColumn ylbl   (Numbers dataY)
          . dataColumn errlbl (Numbers dataErr)
          $ []
    -- Layer com as barras de erro
    layerRange = [
      mark ErrorBar [MTicks []], -- Inclui as bordas
        encoding
        -- Define posições relativas aos eixos x, y e erro para a barra de erro
        . position X [PName xlbl, PmType Nominal]
        . position Y [PName ylbl, PmType Quantitative, PScale [SZero False]]
        . position YError [PName errlbl]
        -- Estabelece a cor da barra conforme a cor da série na qual ela está
        . color [MName catlbl, MmType Nominal]
        $ []
      ]
    -- Layer com as séries própriamente ditas
    fontsize = PAxis [AxLabelFontSize 14, AxTitleFontSize 20]
    layerData = [
      mark Line [], -- Gráfico usando linhas
      encoding
        . position X [PName xlbl, PmType Nominal, fontsize]
        . position Y [PName ylbl, PmType Quantitative, fontsize, PScale [SZero False]]
        . color [MName catlbl, MmType Nominal, MLegend [LLabelFontSize 16]]
        $ []
      ]

main :: IO ()
main = do
  -- ------------------------------
  -- Passo 0 - Leitura dos arquivos
  -- ------------------------------

  arquivos <- mapM readFile ["dados_diarios_rj_inmet.txt", "dados_diarios_sp_inmet.txt"]

  let
    -- ---------------------------------
    -- Passo 1 - Processamento e análise
    -- ---------------------------------

    -- Faz o processamento do arquivo
    -- A princípio deveríamos pegar os Nothings e reportar
    -- as linhas com erro e não "explodir" como está ocorrendo abaixo.
    -- Outra solução é usar o catMaybes e ignorar os erros.
    arqs =  map (map fromJust . processaArquivo) arquivos
    -- Calcula as medidas de interesse.
    mediaTempSeco = map (calculaMediaPorAno temperaturaSeco) arqs
    desvioTempSeco = map (calculaDesvioPorAno temperaturaSeco) arqs
    mediaTempUmido = map (calculaMediaPorAno temperaturaUmido) arqs
    desvioTempUmido = map (calculaDesvioPorAno temperaturaUmido) arqs


    -- ---------------------------------
    -- Passo 2 - Plotagem dos resultados
    -- ---------------------------------

    -- n amostras por cidade
    [nrj, nsp] = map length mediaTempSeco

    -- Cria coluna com as cidades e significado das medições
    colCidade = concatMap (uncurry replicate)
      [(nrj, "RJ - Bulbo seco"),
       (nsp, "SP - Bulbo seco"),
       (nrj, "RJ - Bulbo umido"),
       (nsp, "SP - Bulbo umido")]

    -- Cria coluna com os anos das medições
    colAno' = concatMap (map (fromIntegral . fst))
    colAno = colAno' mediaTempSeco ++ colAno' mediaTempUmido

    colVals = concatMap (map snd)
    -- Cria a coluna com os valores das medições
    colTemp = colVals mediaTempSeco ++ colVals mediaTempUmido
    -- Coluna contendo os desvios de cada medição
    colDesvio = colVals desvioTempSeco ++ colVals desvioTempUmido

  -- Gera o gráfico em um arquivo html.
  toHtmlFile "temperaturas_anuais.html" (
    plot
      768 1024                     -- altura largura
      colCidade                    -- Series
      colAno                       -- Valores eixo x
      colTemp                      -- Valores eixo y
      colDesvio                    -- Desvios
      "Média anual de temperatura" -- Título
      "Cidade"                     -- Título legenda séries
      "Ano"                        -- Título eixo x
      "Média °C"                   -- Título eixo y
      "Desvio")                    -- Título eixo erro
