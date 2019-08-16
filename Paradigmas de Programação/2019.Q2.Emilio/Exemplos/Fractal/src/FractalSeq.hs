-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA

-- Código escrito para o curso de Paradigmas de Programação na UFABC.
-- Este material pode ser usado livremente desde que sejam mantidos,
-- além deste aviso, os créditos aos autores e instituições.

{-# LANGUAGE BangPatterns #-}
module FractalSeq where

import Data.Complex
import Data.Vector (generate, Vector, (!))

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as SBZ (writeFile)
import Data.Binary.Put
import Data.ByteString.Char8 (pack)
import Data.Word
import Text.Printf

-- Parâmetros de execução

-- Limites do plano complexo
maxComplex :: Double
maxComplex = 2.0

-- Limite para bailout da iteração
bailout :: Double
bailout = 4.0
-- Limite de iterações
maxIters :: Int
maxIters = 1000
-- Cores da paleta
nCores :: Int
nCores = 256
-- Nome do arquivo gerado
nomeArquivo :: FilePath
nomeArquivo = "mandel.ppm"

data RGB = RGB Word8 Word8 Word8

magnitudeSq :: Complex Double -> Double
magnitudeSq (r :+ i) = r * r + i * i

-- Calcula o número de iterações para o ponto complexo c
-- mandelIters :: Int -> Complex -> Complex -> Int
mandelIters :: Int -> Complex Double -> Complex Double -> Int
mandelIters iter c z
  | iter > maxIters || magnitudeSq z > bailout = iter `mod` nCores
  | otherwise =
    mandelIters (iter + 1) c (z * z + c)

-- Converte coordenadas da imagem para o plano complexo e calcula o
-- número de iterações daquele ponto
mandelPixel :: Double -> Double -> Int -> Int -> Int
mandelPixel sz2 !fator x y =
  mandelIters 1 (ix' :+ iy') (0 :+ 0)
  where
    ix = (fromIntegral x - sz2) * fator
    iy = (fromIntegral y - sz2) * fator
    -- Apenas um ajuste para mostrar uma região interessante da imagem
    ix' = ix / 8 - 0.5
    iy' = iy / 8 + 0.5

-- Cálculo totalmente arbitrário para o número de cores. O mais
-- natural talvez fosse utilizar uma imagem em tons de cinza baseados
-- no número de iterações. Mas colorido fica mais divertido! :)
paleta :: Vector RGB
paleta =
  generate nCores cor
  where
    cor i = RGB (r i) (g i) (b i)
    r i = fromIntegral $ (255 - i * 2) `mod` 256
    g i = fromIntegral $ (i * 11)      `mod` 256
    b i = fromIntegral $ (i * 7)       `mod` 256

-- Dada a lista de pixels converte em bytes para a ecrita em formato
-- PPM binário
serializa :: Int -> [RGB] -> Put
serializa sz pxs = do
  putByteString header
  putByteString bytes
  where
    header = pack $ printf "P6\n%d %d\n255\n" sz sz
    rgbToList (RGB r g b) = [r, g, b]
    bytes = SB.pack $ concatMap rgbToList pxs

fractal :: Int -> [RGB]
fractal sz =
  [px x y | y <- [0..sz-1], x <- [0..sz-1]]
  where
    sz2 = fromIntegral sz / 2
    -- Fator de conversão imagem plano complexo
    fator = maxComplex * 2 / fromIntegral sz
    px x y = paleta ! mandelPixel sz2 fator x y

main :: IO ()
main = do
  let sz = 1024
  SBZ.writeFile nomeArquivo $ runPut (serializa sz $ fractal sz)
  putStrLn "Feito!"
