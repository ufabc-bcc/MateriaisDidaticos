module Collision where

import Ball
import Blocks
import Window
import Player

-- | Função para if-then-else.
funIf :: Bool -> a -> a -> a
funIf b x y = if b then x else y

-- | Multiplica elementos de duas tuplas.
mulTuple :: Num a => (a, a) -> (a, a) -> (a, a)
mulTuple (x1,x2) (y1,y2) = (x1*y1, x2*y2)

-- | Verifica se tem interseção entre duas faixas de valores.
overlap :: Ord a => (a, a) -> (a, a) -> Bool
overlap (xmin, xmax) (ymin, ymax) = xmin <= ymax && ymin <= xmax

-- | Cria uma faixa de valores centrado em x e com raio r.
range :: Num a => a -> a -> (a, a)
range x r = (x - r, x + r)

-- | Retorna se a bola colidiu com uma das bordas.
topCollision :: Position -> Bool
topCollision (x, y)   = y + 2 * ballSize >   halfHeight

leftCollision :: Position -> Bool
leftCollision (x, y)  = x - 2 * ballSize <= -halfWidth

rightCollision :: Position -> Bool
rightCollision (x, y) = x + 2 * ballSize >=  halfWidth

-- | Retorna se a bola colidiu com o jogador.
paddleCollision :: Float -> Position -> Bool
paddleCollision playerX (x, y) = yCollision && xCollision
  where
    yCollision = y - ballSize <= playerY  && y - 1 >= playerY
    xCollision = x >= playerX - 25 && x <= playerX + 25

-- | Verifica se atinge a borda de algum bloco
inCorner :: (Num a, Ord a) => a -> (a, a) -> (a, a) -> Bool
inCorner x (xmin, xmax) (rmin, rmax) = (xmin > x + rmin && xmin < x + rmax)
                                    || (xmax < x - rmin && xmax > x - rmax)
         
-- | Altera a velocidade da bola ao destruir bloco.
blockCollision :: Position -> Position -> Blocks -> Position
blockCollision v (xball, yball) bs = foldl changeVel v bs
  where
    changeVel (vx, vy) (Block (xb, yb) c) | hitCornerH xb && overlapY yb = (-vx,  vy)
                                          | hitCornerV yb && overlapX xb = ( vx, -vy)
                                          | otherwise                    = ( vx,  vy)
  
    hitCornerH xb = inCorner xb xballRange (0.8*bHalfWidth, bHalfWidth)
    hitCornerV yb = inCorner yb yballRange (-bHalfHeight, -0.8*bHalfHeight)
    overlapY   yb = overlap yballRange $ range yb bHalfHeight
    overlapX   xb = overlap xballRange $ range xb bHalfWidth
    xballRange    = range xball ballSize
    yballRange    = range yball (-ballSize)

-- | Remove blocos atingidos.
removeBlocks :: Blocks -> Position -> Blocks
removeBlocks bs (xball, yball)  = filter (not.hit) bs
  where 
    hit (Block (xb, yb) c) = overlapBallX (range xb bHalfWidth) && overlapBallY (range yb bHalfHeight)
    
    xballRange             = range xball ballSize
    yballRange             = range yball ballSize                          
    overlapBallX           = overlap xballRange
    overlapBallY           = overlap yballRange


-- | Detecta colisão da bola com o jogador, alterando sua velocidade.
paddleBounce :: Position -> Position -> Float -> Float -> Position
paddleBounce bp bv pp pv | paddleCollision pp bp = (vx + 0.3*pv, -vy)
                         | otherwise             = bv 
  where (vx, vy) = bv

  -- | Detecta colisão da bola com as bordas e atualiza velocidade.
wallBounce :: Position -> Position -> Position
wallBounce pos (vx, vy) | sideCollision    = (-vx,  vy)
                        | topCollision pos = ( vx, -vy)
                        | otherwise        = ( vx,  vy)
  where sideCollision = (leftCollision pos || rightCollision pos) 
                          && not (topCollision pos)
