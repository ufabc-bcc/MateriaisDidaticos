-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA

-- Código escrito para o curso de Paradigmas de Programação na UFABC.
-- Este material pode ser usado livremente desde que sejam mantidos,
-- além deste aviso, os créditos aos autores e instituições.

{-# LANGUAGE BangPatterns #-}
module FractalPar where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Par


import Data.Complex
import Data.Word
import Data.Vector (generate, Vector, (!))

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as SBZ (writeFile)
import Data.Binary.Put
import Data.ByteString.Char8 (pack)

import Text.Printf
import System.Environment

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

instance NFData RGB where
  rnf (RGB r g b) = r `seq` g `seq` b `seq` ()

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
serializa :: Int -> [[RGB]] -> Put
serializa sz pxs = do
  putByteString header
  mapM_ putByteString bytess
  -- putByteString bytes
  where
    header = pack $ printf "P6\n%d %d\n255\n" sz sz
    rgbToList (RGB r g b) = [r, g, b]
    bytess = map (SB.pack . concatMap rgbToList) pxs
    -- bytes = SB.pack $ map (concatMap rgbToList) pxs

fractal :: (Int -> Int -> RGB) -> Int -> Int -> Int -> [RGB]
fractal px sz f t  =
  [px x y | y <- [f..t], x <- [0..sz-1]]

fractalPar :: Int -> Int -> [[RGB]]
fractalPar !sz nchunks =
  runPar $ do
     is <- replicateM nchunks new
     mapM_ (\(i, c) -> fork $ put i (fracfun c)) (zip is cs)
     mapM get is
  where
    step = sz `div` nchunks
    chunks l = (l + 1, l + step) : chunks (l + step)
    cs = take nchunks $ chunks (-1)
    sz2 = fromIntegral sz / 2
    -- Fator de conversão imagem plano complexo
    fator = maxComplex * 2 / fromIntegral sz
    px x y = paleta ! mandelPixel sz2 fator x y
    fracfun = uncurry $ fractal px sz


main :: IO ()
main = do
  [ssz,snchunks] <- getArgs
  let sz = read ssz :: Int
  let nchunks = read snchunks :: Int
  SBZ.writeFile nomeArquivo $ runPut (serializa sz $ fractalPar sz nchunks)
  putStrLn "Feito!"
