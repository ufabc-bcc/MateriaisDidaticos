-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA

-- Código escrito para o curso de Paradigmas de Programação na UFABC.
-- Este material pode ser usado livremente desde que sejam mantidos,
-- além deste aviso, os créditos aos autores e instituições.

module Main where

import Data.Bits
import Data.Sequence hiding (take, reverse)

---------------------------------
---------------------------------
-- Definindo as transformações --
---------------------------------
---------------------------------

-- Meu state transformer
newtype ST s a = ST (s -> (a, s))

-- Roda o state transformer com o estado (ambos recebidos por
-- parâmetro) e devolve uma tupla com o resultado da execução e o novo
-- estado
rodaCom :: ST s a -> s -> (a, s)
rodaCom (ST st) s = st s

-- Veja slides da aula para maiores explicações
instance Functor (ST s) where
  -- fmap :: (a -> b) -> ST a -> ST b
  -- x :: a
  fmap g st = ST stb
    where
      stb s = (g x, s')
        where
          (x, s') = rodaCom st s

-- Veja slides da aula para maiores explicações
instance Applicative (ST s) where
  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = ST stb
    where
      stb s = (f x, s'')
        where
          (f, s')  = rodaCom stf s
          (x, s'') = rodaCom stx s'

  pure x = ST (\s -> (x, s))

-- Veja slides da aula para maiores explicações
instance Monad (ST s) where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = ST stb
    where
      stb s = rodaCom (f x) s'
        where (x, s') = rodaCom st s


-------------------------------------------------------------
-------------------------------------------------------------
-- Exemplo 1: Uso de state e gerador de números aleatórios --
-------------------------------------------------------------
-------------------------------------------------------------


-----------------------------------
-- Gerador de números aleatórios --
-----------------------------------

type RandomState = (Int, Int)

simpleRandomInit :: Int -> RandomState
simpleRandomInit seed =
  (z, w)
  where
    n1 = (seed * 48947) `mod` 4294967296;
    z = if n1 /= 0 then n1 else 362436069
    n2 = (seed * 104623) `mod` 4294967296;
    w = if n2 /= 0 then n2 else 521288629


-- Este simples gerador de números aleatórios é baseado naquele
-- proposto por George Marsaglia (MWC - multiply with carry). Apesar
-- de ser muito simples, ele é capaz de passar pela série de testes
-- Marsaglia's DIEHARD para geradores de números aleatórios. A
-- implementação abaixo é uma adaptação daquela feita por John
-- D. Cook.
nextInt :: RandomState -> (Int, RandomState)
nextInt (z, w) =
  (u, (z', w'))
  where
    z' = 36969 * (z .&. 65535) + (z `shiftR` 16)
    w' = 18000 * (w .&. 65535) + (w `shiftR` 16);
    u  = (z `shiftL` 16) + w;


---------------------------
-- Utilização aleatórios --
---------------------------

-- Usa o gerador como função pura
randomList :: Int -> [Int]
randomList seed =
  let
    s0 = simpleRandomInit seed
    (v0, s1) = nextInt s0 -- precisa carregar o estado
    (v1, s2) = nextInt s1 -- entre cada uma das chamadas
    (v2, _) = nextInt s2 in
  [v0, v1, v2]

randomListSize :: Int -> Int -> [Int]
randomListSize seed n =
  -- Para carregar o estado para um número arbitrário de chamadas usa
  -- um fold. Poderia ser também uma versão recursiva.
  reverse . fst $ foldl
    (\(res, s) _ -> let (v, s') = nextInt s in (v:res, s'))
    ([], simpleRandomInit seed)
    [1..n]


-- Para usar o gerador como Monad, é preciso um transformador do tipo
-- certo. Encapsula o transformador no ST usando o construtor Sx
nextIntM :: ST RandomState Int
nextIntM = ST nextInt

-- Devolve um transformador de estados ST [Int] que quando executado
-- com a função rodaCom devolverá ([Int], State) Para ser justo (e
-- comparável com a função randomList) não usa mapM. A próxima função
-- corrige isto.
randomListM' :: ST RandomState [Int]
randomListM' = do
    v0 <- nextIntM
    v1 <- nextIntM
    v2 <- nextIntM
    return [v0, v1, v2]

randomListM :: Int -> [Int]
randomListM seed =
  -- - rodaCom roda o transformador de estados randomList2' tendo como
  --   estado inicial o resultado de (simpleRandomInit seed)
  -- - fst "tira" o resultado do (a, State) = Valor + contexto
  fst $ rodaCom randomListM'(simpleRandomInit seed)

-- Dado n, a quantidade de números aleatórios a serem gerados, devolve
-- um transformador de estados que quando executado com a função
-- rodaCom devolve um ([Int], State)
randomListSizeM' :: Int -> ST RandomState [Int]
randomListSizeM' n = mapM (const nextIntM) [1..n]

-- Código praticamente idêntico ao randomList2
randomListSizeM :: Int -> Int -> [Int]
randomListSizeM seed n =
  fst $ rodaCom (randomListSizeM' n) (simpleRandomInit seed)


-------------------------------------------------
-------------------------------------------------
-- Exemplo 2: Geração dos números de fibonacci --
-------------------------------------------------
-------------------------------------------------

-- No caso de Fibonacci precisamos guardar os 2 últimos números no
-- estado. O transformador avança a sequência e tem o próximo elemento
-- da sequência

fibos :: [Int]
fibos =
  -- O estado inicial está embutido em (0, 1), a saída correspondente
  -- está em 0 : 1. Aplica o state tranformer de múltiplos passos ao
  -- estado inicial
  0 : 1 : fst (rodaCom fibos' (0, 1))
  where
    -- state transformer de um passo
    nextFibo = ST (\ (n1, n2) -> (n1 + n2, (n2, n1 + n2)))
    -- state transformer de múltiplos passos baseado em lista
    fibos' = mapM (const nextFibo) ([1..] :: [Int])


-----------------------
-----------------------
-- Exemplo 3: Primos --
-----------------------
-----------------------

-- No caso de primos, o contexto que queremos guardar são os primos já
-- encontrados. Assim podemos usar esse contexto para calcular o
-- próximo primo

-- Versao 1 - Listas
type PrimeStateL = (Int, [Int]) -- Último encontrado, lista

initPrimes :: PrimeStateL
initPrimes = (3, [2,3])

prime :: Int -> [Int] -> Bool
prime x menores =
  all (\e -> x `mod` e /= 0) cands
  where
    lim = floor $ sqrt (fromIntegral x :: Float)
    cands = takeWhile (<= lim) menores

-- State transformer de um passo
nextPrimeL :: ST PrimeStateL Int
nextPrimeL = ST (\ (ult, ps) ->
               let p = head [x | x <- [ult+2, ult+4..], prime x ps] in
                 (p, (p, ps ++ [p]))) -- a lista de primos é guardada
                                      -- na ordem inversa (não lazy!)

primes :: Int -> [Int]
primes n =
  snd $ snd (rodaCom primes' initPrimes)
  where
    primes' = mapM (const nextPrimeL) [1..n]

-- Versao 2 - Sequence
type PrimeStateS = Seq Int -- Sequence

initPrimes2 :: PrimeStateS
initPrimes2 = fromList [2,3]

prime2 :: Int -> Seq Int -> Bool
prime2 x menores =
  all (\e -> x `mod` e /= 0) cands
  where
    lim = floor $ sqrt (fromIntegral x :: Float)
    cands = takeWhileL (<= lim) menores

nextPrimeS :: ST PrimeStateS Int
nextPrimeS = ST (\s ->
                    let
                      lst ~(_ :|> l) = l
                      ult = lst s
                      p = head [x | x <- [ult+2, ult+4..], prime2 x s] in
                      (p, s :|> p))

primes2 :: Int -> [Int]
primes2 n =
  2 : 3 : fst (rodaCom primes' initPrimes2)
  where
    primes' = mapM (const nextPrimeS) [1..n]



main :: IO ()
main = do
  putStrLn "Aleatórios"
  print $ randomList 42
  print $ randomListSize 42 3
  print $ randomListM 42
  print $ randomListSizeM 42 3

  putStrLn "\nFibos"
  print $ take 20 fibos

  putStrLn "\nPrimos"
  print $ primes 20
  print $ primes2 20
