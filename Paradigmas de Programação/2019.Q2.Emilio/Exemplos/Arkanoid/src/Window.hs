module Window where

import Graphics.Gloss

-- Configuração da tela

-- | Largura da janela.
width :: Int
width = 800

-- | Altura da janela.
height :: Int
height = 600

-- | Posição da janela.
offset :: Int
offset = 100

-- | Para uso nas funções de colisão.
-- Esse valor é o extremo da tela
halfWidth :: Float
halfWidth = fromIntegral width / 2

halfHeight :: Float
halfHeight = fromIntegral height / 2

-- | Cor de fundo.
background :: Color
background = black

-- | Janela do jogo.
window :: Display
window = InWindow "Arkanoid" (width, height) (offset, offset)

-- Bordas.

-- | Bordas laterais
sideWall :: Float -> Picture
sideWall offset = translate offset 0 
                $ color wallColor 
                $ rectangleSolid 10 
                $ fromIntegral height

-- | Borda de cima.
topWall :: Picture
topWall = translate 0 halfHeight
        $ color wallColor 
        $ rectangleSolid (fromIntegral width + 10) 10

-- | Cor da borda.
wallColor :: Color
wallColor = greyN 0.5

-- | Imagem das bordas.
walls :: Picture
walls = pictures [sideWall halfWidth, sideWall (-halfWidth), topWall]  

-- Textos

-- | Mensagem atual a ser mostrada.
curMsg :: Int -> Bool -> Picture
curMsg  0   paused = pauseMsg paused
curMsg (-1) paused = lostMsg
curMsg  _   paused = winMsg

winMsg    = renderTxt green "You won! (r = new game)"
lostMsg   = renderTxt red   "Git gud! (r = new game)"
 
pauseMsg True  = renderTxt blue "Press p to play!"
pauseMsg False = renderTxt blue ""
       
-- | Escreve texto na tela
renderTxt c t = translate (-150) 150 
              $ scale 0.3 1 
              $ color c 
              $ text t
