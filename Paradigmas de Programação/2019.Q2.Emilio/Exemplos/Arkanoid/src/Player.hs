module Player where

import Graphics.Gloss
import Window

-- Informações do jogador

-- | Cor do jogador.
playerColor :: Color
playerColor = blue

-- | Tamanho do jogador.
playerWidth :: Float
playerWidth = 50

playerHeight :: Float
playerHeight = 10

-- | Posição do jogador no eixo y
playerY :: Float
playerY = -250

-- | Imagem do jogador.
mkPlayer :: Float -> Picture
mkPlayer x = translate x playerY
           $ color playerColor 
           $ rectangleSolid playerWidth playerHeight
           
           
-- | Atualiza posição do jogador.
movePlayer :: Float -> Float -> Float -> Float
movePlayer seconds x v | rightWallCollision x && v > 0 = x - 1
                       | leftWallCollision  x && v < 0 = x + 1
                       | otherwise                     = deltaX
  where deltaX = x + v * seconds

               
-- | Verifica se o jogador atingiu a parede da esquerda.
leftWallCollision :: Float -> Bool
leftWallCollision x | x - 25 <= -halfWidth + 5  = True
                    | otherwise                 = False

-- | Verifica se o jogador atingiu a parede da direita.
rightWallCollision :: Float -> Bool
rightWallCollision x | x + 25 >=  halfWidth - 5  = True
                     | otherwise                 = False

-- | Verifica se o jogador atingiu a parede.
paddleWallCollision :: Float -> Bool
paddleWallCollision x = leftWallCollision x || rightWallCollision x
