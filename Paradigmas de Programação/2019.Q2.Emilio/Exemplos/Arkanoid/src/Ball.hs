module Ball where

import Graphics.Gloss

-- | Coordenada do centro da bola.
type Position = (Float, Float)

-- Propriedades da bola

-- | Raio da bola.
ballSize :: Float
ballSize = 5

-- | Cor da bola.
ballColor :: Color
ballColor = dark red

-- | Cria a imagem da bola no estado atual do jogo.
ball :: Position -> Picture
ball (x, y) = translate x y
            $ color ballColor 
            $ circleSolid ballSize
            
-- | Atualiza posição da bola.
moveBall :: Float -> Position -> Position -> Position
moveBall seconds (x, y) (vx, vy) = (x', y')
  where x' = x + vx * seconds
        y' = y + vy * seconds
        

