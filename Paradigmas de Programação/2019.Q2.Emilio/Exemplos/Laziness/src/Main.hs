-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA

-- Código escrito para o curso de Paradigmas de Programação na UFABC.
-- Este material pode ser usado livremente desde que sejam mantidos,
-- além deste aviso, os créditos aos autores e instituições.

module Main where

import Prelude hiding ((*>), (^))
import Data.Char
import Data.Ratio
import Data.Vector ((!), generate)

import Text.Printf
import System.CPUTime
import System.IO

primosCandidatos1,primosCandidatos2 :: [Int]
-- Números ímpares
primosCandidatos1 = [5,7..]
-- Primos >= 5 só podem ser da forma 6k+-1
primosCandidatos2 =
  junta [5,11..] [7,13..]
  where
    -- Dá pra fazer com zip e concat! Tente!
    -- Note também o primeiro uso de ~ que indica que o único pattern
    -- possível é o fornecido. Caso seja recebido um [], por exemplo
    -- causaria um erro em tempo de execução.
    junta ~(a:as) ~(b:bs) = a : b : junta as bs

primos0 :: [Int] -> [Int] -> [Int]
primos0 cands primos =
  2 : 3 : filter (ePrimo $ primos0 cands primos) cands

primos1 :: [Int]
primos1 = primos0 primosCandidatos1 primos1

primos2 :: [Int]
primos2 = primos0 primosCandidatos2 primos2

ePrimo :: [Int] -> Int -> Bool
ePrimo primos x =
  all (\e -> x `rem` e /= 0) possiveisFatores
  where
    maxDiv = ((floor . sqrt) :: Double -> Int) $ fromIntegral x
    possiveisFatores = takeWhile (<= maxDiv) primos

-- Retira indireções para acelerar o processo
primos2' :: [Int]
primos2' =
  primos2''
  where
    primos2'' = 2 : 3 : filter ePrimo' primosCandidatos2
    ePrimo' x =
      all (\e -> x `rem` e /= 0) possiveisFatores
      where
        maxDiv = ((floor . sqrt) :: Double -> Int) $ fromIntegral x
        possiveisFatores = takeWhile (<= maxDiv) primos2''

-- Implementação tradicional e ineficiente
fibo1 :: Int -> Integer
fibo1 0 = 0
fibo1 1 = 1
fibo1 n = fibo1 (n - 1) + fibo1 (n - 2)

-- Versão memoizada/persistente
fibo2 :: Int -> Integer
fibo2 = (map fib' [0..] !!)
     where fib' 0 = 0
           fib' 1 = 1
           fib' n = fibo2 (n - 1) + fibo2 (n - 2)

-- Versão memoizada com vetor
fibo3 :: Int -> Integer
fibo3 n =
  fibos ! n
  where
    fibos = generate (n + 1) fibo3'
    fibo3' 0 = 0
    fibo3' 1 = 1
    fibo3' i = fibos ! (i - 1) + fibos ! (i - 2)

-- Versão memoizada com lista
fibo4 :: Int -> Integer
fibo4 n =
  fibos !! n
  where
    fibos = 0:1: zipWith (+) fibos (tail fibos)


{- -------------------------------------------------------------------
----------------------------------------------------------------------
O código abaixo foi baseado no paper "The most unreliable technique in
the world to compute pi". As explicações sobre o funcionamento das
funções podem ser encontradas em
https://karczmarczuk.users.greyc.fr/arpap/lazypi.pdf
-----------------------------------------------------------------------
----------------------------------------------------------------------}

{- Infloat (INfinite precision FLOAT) - Guarda a expressão expandida
   de um número, o primeiro elemento guarda a parte inteira e os
   demais são os dígitos de sua expansão (base 16 por padrão)
-}
newtype Infloat = Infloat [Integer]

-- Força a execução da operação de IO
printFlush :: Show a => a -> IO()
printFlush s =  do
    putStr $ show s
    hFlush stdout

-- Versão ávida de print para Infloats
printFlushMF :: Infloat -> IO()
printFlushMF ~(Infloat (d:ds)) = do
    printFlush d
    putStr "."
    mapM_ printFlush ds

zeros :: [Integer]
zeros = repeat 0

base,base1 :: Integer
base = 16
base1 = base - 1

-- Desempacota Infloat
(^) :: Infloat -> [Integer]
(^) (Infloat a) = a

instance Show Infloat where
  show x =
    show d ++ "." ++ map (intToDigit . fromInteger) (take 100 ds) ++ "..."
    where
      Infloat (d:ds) = converteBase x 10

instance Num Infloat where

  (Infloat a) + (Infloat b) =
    Infloat $ cpr w ws
    where
      (w:ws) = zipWith (+) a b
      cpr d0 ~(d1:ds) -- Note novamente o uso de ~
        -- É aqui que a mágica da propagação do "vai 1" é feita de
        -- maneira preguiçosa. Esta função é parcial e explode em um
        -- caso bem específico, qual?
        | d1 < base1 = d0 : cpr d1 ds
        | d1 > base1 = d0 + 1 : cpr (d1 - base) ds
        | otherwise =
            let t@(t0:ts) = cpr d1 ds in
              if t0 < base
              then d0 : t
              else d0 + 1 : (t0 - base) : ts

  -- A representação que escolhemos para Infloats torna a
  -- implementação da * nada trivial (é um ótimo exercício pensar como
  -- isso poderia ser feito e quais seriam as limitações!). Olhe a função
  -- (*>) para inspiração.
  _ * _ = undefined

  -- Guardamos o sinal apenas na parte inteira
  signum ~(Infloat (d:_)) = Infloat $ signum d : zeros

  -- Apesar do sinal ser guardado na parte inteira, utilizamos uma
  -- representação baseada em complemento de base, logo precisamos
  -- consertar toda a sequência não bastando aplicar abs à cabeça.
  abs ~d@(Infloat (d0:_))
    | d0 < 0 = negate d
    | otherwise = d

  -- Expande o inteiro com zeros após a vírgula
  fromInteger i = Infloat $ fromInteger i : zeros

  -- A ideia abaixo funciona baseada por complemento de base
  -- E, por definição, quebra nossa ED para a soma (Pq?). Tente:
  -- let x = (1 :: Infloat) /> 3 in x + negate x
  negate ~(Infloat (d:ds)) =
    Infloat $ (negate d - 1) : map (base1 -) ds

instance Fractional Infloat where
  fromRational r =
    let  in
      Infloat $ fcn (numerator r) (denominator r)
    where
      fcn n d =
        let (a, b) = quotRem n d in
          a : fcn (base * b) d

  -- É possível implementar usando exatamente a mesma ideia usada em
  -- fromRational. Veja também a implementação de (/>)
  recip _ = undefined

(/>) :: Infloat -> Integer -> Infloat
(Infloat a) /> m =
  Infloat $ dvd 0 a
  where
    dvd acc ~(d0:ds) =
      n : dvd r ds
      where
        (n, r) = quotRem (base * acc + d0) m

(*>) :: Integer -> Infloat -> [Integer]
0 *> _ = zeros
1 *> v = (^) v
m *> ~(Infloat (v0:v1:vs)) =
  cm c0 r0 vs
  where
    (c0, r0) = quotRem (m * (v0 * base + v1)) base
    cm c r ~(u0:us)
      -- Note a semelhança com a soma
      | p < base1 = c : cm p b us
      | p > base1 = c + 1 : cm (p - base) b us
      | otherwise =
        let w@(w0:wq) = cm p b us in
          if w0 > base then c : w
          else (c + 1) : 0 : wq
      where
        (a, b) = quotRem (m * u0) base
        p = a + r

-- Fórmula de Bailey-Borwein-Plouffe
termoBBP :: Integer -> Infloat
termoBBP i =
  fromRational razao
  where
    j = 8 * i
    razao = 4 % (j + 1) - 2 % (j + 4) - 1 % (j + 5) - 1 % (j + 6)

pi16 :: Infloat
pi16 =
  ssum $ map termoBBP [0..]
  where
    a <+> b = (^)(Infloat a + Infloat b)
    ssum a = Infloat $ ssum' $ map (^) a
    -- O pulo do gato é reconhecer que a fórmula de BBP é uma soma
    -- deslocada de um dígito por parcela em base 16
    ssum' ~(u@(u0:u1:uq) : v@(v0:_) : r)
      | u1 + v0 < base1 = u0 : ssum' ((u1:uq) <+> v : r)
      | otherwise = u <+> (0:ssum' (v:r)) -- propaga o vai 1 para a esquerda

converteBase :: Infloat -> Integer -> Infloat
converteBase (Infloat v) nBase =
  Infloat $ converteBase' v
  where
    -- (i) pega o primeiro dígito, (ii) multiplica por base para
    -- deslocar o próximo dígito para antes da vírgula, (iii) repete
    converteBase' ~(v0:vs) =
      v0 : converteBase' (nBase *> Infloat (0:vs))

pi10 :: Infloat
pi10 = converteBase pi16 10

-- Primeiro uso de uma Monad IO fora da main. \o/
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / (10**12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main :: IO ()
main = do
    putStrLn "Primo impar..."
    time $ (primos1 !! 1000000) `seq` return ()

    putStrLn "Primo 6k+-1..."
    time $ (primos2 !! 1000000) `seq` return ()

    putStrLn "Primo 6k+-1 (sem indireção)"
    time $ (primos2' !! 1000000) `seq` return ()

    putStrLn "---"
    putStrLn "---"

    putStrLn "Fibo 1 40..."
    time $ fibo1 40 `seq` return ()

    putStrLn "Fibo 2 40..."
    time $ fibo2 40 `seq` return ()

    putStrLn "Fibo 3 40..."
    time $ fibo3 40 `seq` return ()

    putStrLn "Fibo 4 40..."
    time $ fibo4 40 `seq` return ()

    putStrLn "---"

    putStrLn "Fibo 2 10000..."
    time $ fibo2 10000 `seq` return ()

    putStrLn "Fibo 3 10000..."
    time $ fibo3 10000 `seq` return ()

    putStrLn "Fibo 4 10000..."
    time $ fibo4 10000 `seq` return ()

    putStrLn "---"
    putStrLn "---"

    putStrLn "Pi 1k dígitos.."
    time $ ((^) pi10 !! 1000)  `seq` return ()

    putStrLn "---"

    putStrLn "Pi - Todos os dígitos (ou até você cansar ou ficar sem memória). Ctrl+C para parar..."
    printFlushMF pi10
