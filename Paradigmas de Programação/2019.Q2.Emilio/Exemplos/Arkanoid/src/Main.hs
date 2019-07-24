-- | Arkanoid game implemented in Haskell.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Window
import Game

-- Propriedades da animação

-- | Número de frames por segundo.
fps :: Int
fps = 60        
    
-- | Window creation.
main :: IO ()
main = play window background fps initialState render handleKeys update 
-- Para mostrar apenas a janela:
-- display window background (render initialState) -- 
