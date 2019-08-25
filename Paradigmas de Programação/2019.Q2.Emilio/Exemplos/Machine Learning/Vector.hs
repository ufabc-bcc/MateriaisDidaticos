{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  NoMonomorphismRestriction #-}
{-|
Module      : Vectors
Description : operações com vetores
Copyright   : (c) Fabrício Olivetti, 2018
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Exemplifies the use of Maybe type
-}

module Vector where

import Data.List (transpose, foldl1')

type Vector a = [a]

type Matrix a = [[a]]

-- scalar - vector operators
(.+) xs x = map (+x) xs
(.-) xs x = map ((-)x) xs
(.*) xs x = map (*x) xs
(./) xs x = map (/x) xs
(.^) xs x = map (^x) xs
(.**) xs x = map (**x) xs

(+.) x xs = map (+x) xs
(*.) x xs = map (*x) xs

-- vector - vector operators
(.+.) = zipWith (+)
(.-.) = zipWith (-)
(.*.) = zipWith (*)
(./.) = zipWith (/)

-- scalar - matrix operators
(..+) xs x = map (.+x) xs
(..-) xs x = map ((.-)x) xs
(..*) xs x = map (.*x) xs
(../) xs x = map (./x) xs
(..^) xs x = map (.^x) xs
(..**) xs x = map (.**x) xs

(+..) x xs = map (x+.) xs
(*..) x xs = map (x*.) xs

-- vector - matrix operators
(..+.) xs x = map (.+.x) xs
(..-.) xs x = map (.-.x) xs
(..*.) xs x = map (.*.x) xs
(.*..) x xs = map (.*.x) xs
(../.) xs x = map (./.x) xs

-- matrix - matrix operators
(..+..) = zipWith (.+.)
(..-..) = zipWith (.-.)
(..*..) = zipWith (.*.)
(../..) = zipWith (./.)

dotprod :: Num a => Vector a -> Vector a -> a
dotprod u v = sum $ u .*. v

norm u = dotprod u u

mediaVetor :: [[Double]] -> [Double]
mediaVetor xs = sumVec ./ n
  where
    sumVec = foldl1' (.+.) xs
    n      = fromIntegral $ length xs

mapColunas :: (Vector a -> Vector a) -> Matrix a -> Matrix a
mapColunas f m = map f $ transpose m

outerprod :: Num a => Vector a -> Vector a -> Matrix a
outerprod u v = [ [ui * vi | vi <- v] | ui <- u ]

reshape :: Int -> Vector a -> Matrix a
reshape n [] = []
reshape n xs = (take n xs) : (reshape n (drop n xs))

length' xs = fromIntegral $ length xs

