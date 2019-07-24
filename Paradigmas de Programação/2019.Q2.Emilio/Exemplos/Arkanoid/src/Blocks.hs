module Blocks where

import Graphics.Gloss
import Ball

-- Propriedades dos blocos
-- | Blocos por fileira
blocksPerRow :: Int
blocksPerRow = 15

-- | Tamanho dos blocos.
blockSize :: (Float, Float)
blockSize = (20, 10)

bHalfWidth :: Float
bHalfWidth  = 1 + fst blockSize / 2

bHalfHeight :: Float
bHalfHeight = 1 + snd blockSize / 2

-- | Informação dos blocos.
data BlockInfo = Block
  { blockPos :: Position -- ^ (x, y) coordenada do bloco.
  , blockCol :: Color    -- ^ cor do bloco.
  }

-- | Lista dos blocos atuais.
type Blocks = [BlockInfo]

-- | Desenha os blocks.
drawBlocks :: Blocks -> Picture
drawBlocks bs = pictures $ map drawBlock bs
  where
    drawBlock (Block (x, y) col) = translate x y (color col block)
    block                        = rectangleSolid w h
    (w, h)                       = blockSize

-- | Gera um dos 60 blocos iniciando da coordenada (-250, 100)
genBlock :: Int -> BlockInfo
genBlock n     = Block { blockPos = pos,  blockCol = orange }
  where pos    = (fromIntegral bx, fromIntegral by)
        bx     = -250 + x * 35                -- x0 + x * espaco
        by     =  100 - y * 40                -- y0 + y * espaco
        (y, x) = n `divMod` blocksPerRow
        
-- | Verifica se ainda existem blocos a serem destruídos.
hasBlocks :: Blocks -> Bool
hasBlocks blocks = not (null blocks)