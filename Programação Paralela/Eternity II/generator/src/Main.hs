-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- Q1/2020

module Main where
import Control.Monad
import Data.Array.IO
import Data.List.Split
import System.Environment
import System.Random
import Text.Printf

-- | https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- nArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    nArray :: Int -> [a] -> IO (IOArray Int a)
    nArray m =  newListArray (1, m)

data Rot = N | E | S | W deriving (Show, Enum)
type Color = Int
data Tile = Tile {
    rotation :: Rot
  , nColor   :: Color
  , eColor   :: Color
  , sColor   :: Color
  , wColor   :: Color
  } deriving Show

rotateTile :: Int -> Tile -> Tile
rotateTile r (Tile _ n e s w) = Tile (toEnum r) n e s w

formatTile :: Tile -> String
formatTile (Tile r n e s w) =
  printf "%d %d %d %d" n' e' s' w'
  where
    [n', e', s', w'] = take 4 $ drop (fromEnum r) $ cycle [n, e, s, w]


data Board = Board {
    boardSize     :: Int
  , boardMaxColor :: Int
  , boardLines    :: [[Tile]]
  } deriving Show

formatBoard :: Board -> String
formatBoard (Board bs bc bls) =
  printf "%s\n%s" header lns
  where
    header = printf "%d %d" bs bc :: String
    lns = unlines [formatTile t | ts <- bls, t <- ts]

gray :: Color
gray = 0

newTile :: Int -> Rot -> Maybe Color -> Maybe Color -> Maybe Color -> Maybe Color -> IO Tile
newTile maxColor rot c0 c1 c2 c3 = do
  c0'<- newC c0
  c1'<- newC c1
  c2'<- newC c2
  c3'<- newC c3
  pure $ Tile rot c0' c1' c2' c3'
  where
    newC = maybe (randomRIO (1, maxColor)) pure

newLine' :: Int -> Int -> Bool -> [Color] -> [Tile] -> IO [Tile]
newLine' 0  _        _   []     ts        = pure ts
newLine' sz maxColor top (c:cs) ts@(t0:_) = do
  t <- newTile maxColor N
    (if top then Just gray else Nothing)
    (Just $ wColor t0)
    (Just c)
    (if sz == 1 then Just gray else Nothing)
  newLine' (sz - 1) maxColor top cs (t:ts)
newLine' _ _ _ _ _ = undefined

newLine :: Int -> Int -> Bool -> [Color] -> IO [Tile]
newLine size maxColor top (c:cs) = do
  fstTile <- newTile maxColor N
    (if top then Just gray else Nothing)
    (Just gray)
    (Just c)
    Nothing
  newLine' (size - 1) maxColor top cs [fstTile]
newLine _ _ _ _ = undefined

newBoard' :: Int -> Int -> Int -> [Color] -> [[Tile]] -> IO Board
newBoard' size 0    maxColor _  acc = pure $ Board size maxColor acc
newBoard' size rows maxColor cs acc = do
    bl <- newLine size maxColor (rows == 1) (reverse cs)
    newBoard' size (rows - 1) maxColor (map nColor bl) (bl:acc)

newBoard :: Int -> Int -> IO Board
newBoard size maxColor =
  newBoard' size size maxColor cs []
  where
    cs = replicate size gray


shuffleBoard :: Board -> IO Board
shuffleBoard (Board sz cl ls) = do
  rots <- replicateM elems (randomRIO (0, 3))
  let rotTiles = zipWith rotateTile rots (concat ls)
  ntiles <- chunksOf sz <$> shuffle rotTiles
  pure $ Board sz cl ntiles
  where
    elems = sz * sz

main :: IO ()
main = do
  [size, maxColors] <- getArgs
  board <- newBoard (read size) (read maxColors) >>= shuffleBoard
  putStr $ formatBoard board
