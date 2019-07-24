module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Ball
import Blocks
import Player
import Collision

-- Informações do jogo

-- | Estado do jogo
data GameStatus = Game
  { ballLoc   :: Position -- ^ (x, y) coordenada da bola.
  , ballVel   :: Position -- ^ (x, y) velocidade da bola.
  , playerLoc :: Float    -- ^ Posição horizontal do jogador.
  , playerVel :: Float    -- ^ Velocidade do jogador.
  , playerAcc :: Float    -- ^ Aceleração do jogador.
  , isPaused  :: Bool     -- ^ Indicador do status de pausa.
  , blocks    :: Blocks   -- ^ Lista de blocos na tela.
  , gameStat  :: Int      -- ^ Status do jogo: 0 - ainda em jogo, 1 - vitória, -1 - derrota.
  }

-- | Estado inicial do jogo.
initialState :: GameStatus
initialState = Game
  { ballLoc   = (0, -100)
  , ballVel   = (25, -150)
  , playerLoc = 0
  , playerVel = 0
  , playerAcc = 150
  , isPaused  = True
  , blocks    = map genBlock [0..59]
  , gameStat = 0
  }
  
  
-- | Converte o estado do jogo em uma imagem de tela.
render :: GameStatus -> Picture
render game = pictures [ballPic, walls, playerPic, blocksPic, msgPic]
  where
    ballPic   = ball       (ballLoc game)
    playerPic = mkPlayer   (playerLoc game)
    blocksPic = drawBlocks (blocks game)
    msgPic    = curMsg     (gameStat game) (isPaused game)
    
-- | Atualiza o estado da bola.
updateBall :: Float -> GameStatus -> GameStatus
updateBall seconds game = game { ballLoc = moveBall seconds pos v }
  where pos = ballLoc game
        v   = ballVel game

-- | Atualiza o estado do jogador.
updatePlayer :: Float -> GameStatus -> GameStatus
updatePlayer seconds game = game { playerLoc = movePlayer seconds x v }
  where x = playerLoc game
        v = playerVel game

-- | Incrementa a velocidade do jogador.
incVel :: GameStatus -> GameStatus
incVel game = game { playerVel = playerVel game + playerAcc game }

-- | Decrementa a velocidade do jogador.
decVel :: GameStatus -> GameStatus
decVel game = game { playerVel = playerVel game - playerAcc game }

-- | Inverte o estado de pausa do jogo.
invPause :: GameStatus -> GameStatus
invPause game = game { isPaused = not $ isPaused game }

-- | Responde aos e ventos de teclas.
handleKeys :: Event -> GameStatus -> GameStatus
-- Tecla 'r' retorna ao estaod inicial.
handleKeys (EventKey (Char 'r') Down _ _)            game = initialState
-- Tecla 'p' pausa e despausa o jogo.
handleKeys (EventKey (Char 'p') Down _ _)            game = invPause game 
-- Tecla '←' move para esquerda.
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _)  game = decVel game
-- Soltar a tecla '←' para o jogador.
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _)    game = incVel game
-- Tecla '→' move o jogador para a direita.
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = incVel game
-- Soltar a tecla '→' para o jogador.
handleKeys (EventKey (SpecialKey KeyRight) Up _ _)   game = decVel game
-- Qualquer outra tecla é ignorada.
handleKeys _ game = game

-- | Atualiza posição da bola de acordo com colisões nas bordas.
updateWall :: GameStatus -> GameStatus
updateWall game = game { ballVel = wallBounce pos v }
  where pos = ballLoc game
        v   = ballVel game
        
-- | Atualiza posição da bola de acordo com colisões com o jogador.
updatePaddle :: GameStatus -> GameStatus
updatePaddle game = game { ballVel = paddleBounce bp bv pp pv }
  where bp = ballLoc   game
        bv = ballVel   game
        pp = playerLoc game
        pv = playerVel game

-- | Atualiza posição da bola de acordo com colisões nos blocos e remove blocos.
updateBlocks :: GameStatus -> GameStatus
updateBlocks game = game { ballVel = ballVel', blocks = blocks' }
  where
    -- atualiza a velocidade da bola ao atingir blocos
    ballVel' = blockCollision (ballVel game) (ballLoc game) (blocks game)
    blocks'  = removeBlocks   (blocks game)  (ballLoc game)
            
-- | Atualiza o estado do jogo.
update :: Float -> GameStatus -> GameStatus
update seconds game | isPaused game                 = game
                    | (not.hasBlocks) $ blocks game = game { gameStat =  1 }
                    | dropped                       = game { gameStat = -1 }
                    | otherwise                     = collisions $ moves game
                    where 
                      dropped    = y < (-halfHeight) - 5
                      y          = snd $ ballLoc game
                      collisions = updatePaddle . updateBlocks . updateWall
                      moves      = updatePlayer seconds . updateBall seconds
