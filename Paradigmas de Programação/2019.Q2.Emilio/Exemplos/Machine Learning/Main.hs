{-|
Compilar com: 
    stack ghc -- Main.hs

Pedro Henrique Arruda Faustini, 2019
pedro.faustini@ufabc.edu.br

GPL-3
-}

module Main
where

import Vector
import Data.List (transpose, sortBy, groupBy, foldl')
import Data.Function (on)
import Data.Ord (comparing)



type Point = ([Double], Double)

data Profissao = Engenheiro | Professor | Gerente | Estudante deriving (Eq, Show, Read, Enum, Bounded)

data Conceito = F | D | C | B | A deriving (Show, Read, Enum)

type Nota = Double

type Objeto = (Profissao, Conceito, Nota)

type Objeto' = [Double]

parseFile :: String -> [Objeto]
parseFile file = map parseLine (lines file)
    where
        parseLine l = toObj (words l)
        toObj [w1, w2, w3] = (read w1 :: Profissao, read w2 :: Conceito, read w3 :: Nota)


binariza :: Profissao -> [Double]
binariza p = map bool2double [p == p' | p' <- profissoes]
    where
        profissoes = [minBound..] :: [Profissao]
        bool2double True = 1.0
        bool2double _ = 0.0

transformData :: [Objeto] -> [Objeto']
transformData data' = map parseObj data'
    where
        parseObj (prof, conc, nota) = (binariza prof) ++ [rank conc, nota]

rank :: Conceito -> Double
rank co = (fromEnum' co) / (fromEnum' A)
    where
        fromEnum' = fromIntegral . fromEnum

padroniza :: [[Double]] -> [[Double]]
padroniza x = transpose $ mapColunas padroniza' x

padroniza' :: [Double] -> [Double]
padroniza' x = devMedia ./ sigma
    where
        media xs = (sum xs) / n
        devMedia = x .- (media x)
        sigma = sqrt $ media $ devMedia .** 2
        n = length' x



sumArrays :: [Double]->[Double]->[Double]
sumArrays a [] = a
sumArrays [] a = a
sumArrays (x:xs) (y:ys) = (x+y) : (sumArrays xs ys)

dcdistance :: [Point] -> [Point]
dcdistance points = [distances x | x <- points]
    where
        points' = groupBy ((==) `on` snd) $ sortBy (comparing snd) points :: [[Point]]
        vrc' = map (map (\x -> fst x)) points'
        vrc = map (foldr (\x -> sumArrays x) []) vrc'
        distances x = (d, label) :: Point
            where
                label = snd x
                f = fst x
                d = map (euclidean f ) vrc



-----------------------------------------------------------------------------------------------------
euclidean x y = sqrt $ sum $ map (\(x, y) -> (x-y)^^2 ) (zip x y)

sortByKey      =  sortBy ordenaTupla
ordenaTupla :: (Ord a) => (a, t) -> (a, t) -> Ordering
ordenaTupla (a1,b1) (a2,b2)
    | a1 < a2 = LT
    | a1 > a2 = GT
    | a1 == a2  = EQ


-----------------------------------------------------------------------------------------------------


knn1 :: [Point] -> [Point] -> [Double]
knn1 train test = map (nearest) test
     where
     nearest t =     snd 
                     $ head
                     $ sortByKey
                     $ map (distance t) train
     distance (t1,y1) (t2,y2) = (euclidean t1 t2, y2)

acuraccy :: [Double] -> [Double] -> Double
acuraccy ytrue ypred = corrects / n_objects
    where
        n_objects = fromIntegral (length ypred) :: Double
        corrects = fromIntegral (length $ filter (\(t, p) -> t == p) (zip ytrue ypred)) :: Double

precision :: Double -> [Double] -> [Double] -> Double
precision targetClass ytrue ypred = tp / (tp + fp)
    where
        correctP = fromIntegral (length $ filter (\(t, p) -> p == targetClass && t == targetClass) (zip ytrue ypred))  :: Double
        objectsP = fromIntegral (length $ filter (\t -> t == targetClass) ytrue)  :: Double
        tp = correctP / objectsP
        classifyN = fromIntegral (length $ filter (\(t, p) -> p == targetClass && t /= targetClass) (zip ytrue ypred)) :: Double
        objectsN = fromIntegral (length $ filter (\t -> t /= targetClass) ytrue)  :: Double
        fp = classifyN / objectsN

recall :: Double -> [Double] -> [Double] -> Double
recall targetClass ytrue ypred = tp / (tp + fn)
    where
        correctP = fromIntegral (length $ filter (\(t, p) -> p == targetClass && t == targetClass) (zip ytrue ypred))  :: Double
        objectsP = fromIntegral (length $ filter (\t -> t == targetClass) ytrue)  :: Double
        tp = correctP / objectsP
        classifyN = fromIntegral (length $ filter (\(t, p) -> p /= targetClass && t == targetClass) (zip ytrue ypred)) :: Double
        fn = classifyN / objectsP



getMediaSigma :: [[Double]] -> [[Double]]
getMediaSigma trainData = transpose $ mapColunas getMediaSigma' trainData

getMediaSigma' :: [Double] -> [Double]
getMediaSigma' x = [media, sigma]
    where
        media = (sum x) / n
        mediav xs = (sum xs) / n
        devMedia = x .- (mediav x)
        sigma = sqrt $ mediav $ devMedia .** 2
        n = length' x

padronizaTestData :: [[Double]] -> [[Double]] -> [[Double]]
padronizaTestData _ [] = []
padronizaTestData media_sigma testData = [padronizaTestData' media sigma (head testData)] ++ (padronizaTestData (tail media_sigma) (tail testData))
    where
        media = head $ head media_sigma
        sigma = (head media_sigma) !! 1

padronizaTestData' :: Double -> Double -> [Double] -> [Double]
padronizaTestData' media sigma x = devMedia ./ sigma
    where
        devMedia = x .- media

dataset_test :: [Point]
dataset_test = [([1, 1], 0), ([2, 1], 2), ([12, 2], 1), ([33, 33], 2)]

dataset_train :: [Point]
dataset_train = [
                ( [0, 1], 0 ),
                ( [0, 2], 0 ),
                ( [0, 3], 0 ),
                ( [10, 0], 1 ),
                ( [11, 0], 1 ),
                ( [12, 0], 1 ),
                ( [25, 25], 2 ),
                ( [26, 26], 2 ),
                ( [27, 27], 2 ),
                ( [1, 0], 1 ),
                ( [2, 0], 1 ),
                ( [3, 0], 1 ),
                ( [0, 10], 0 ),
                ( [0, 11], 0 ),
                ( [0, 12], 0 ),
                ( [12, 3], 2 )]

----------------------------------------------------------------------------------------------------------------------------------------------------------------

silhouette :: [Point] -> Double
silhouette points = sum scores / fromIntegral (length points)
    where
        scores = [silhouette' x points | x <- points ]
        
silhouette' :: Point -> [Point] -> Double
silhouette' x points = if a < b then (1 - (a / b)) else if a > b then ((b/a)- 1) else 0
    where
        filteredGroup = map fst $ filter (\p -> snd p == label) points
        label = snd x
        x' = fst x
        a = (sum $ map (euclidean x') filteredGroup) / fromIntegral ((length filteredGroup)-1)
        otherGroups = groupBy ((==) `on` snd) $ sortBy (comparing snd) (filter (\p -> snd p /= label) points) :: [[Point]]
        distancesClusters = map (meanDistanceCluster x) otherGroups
        b = minimum distancesClusters
        meanDistanceCluster x cluster = (sum $ map (euclidean x') (map fst cluster) ) / fromIntegral (length cluster)

        


----------------------------------------------------------------------------------------------------------------------------------------------------------------

dataset_train3 = [[0, 1.1, 3],[3, 2.1, 1.3],[0.7, 1.5, 1],[2.6, 3.1, 2.2],[2.7, 0.1, 0.3],[1.7, 2.1, 1.1],[1.1, 0, 0.7],[0, 1, 3],[0, 0, 0],[1, 1.1, 3]]
ytrue3 = [1.1,0.5,1,2.5,2.3,4.3,2,3,3.4,2.1]


apply :: [Double] -> [Double] -> [Double]
apply weights points = map (\(w,x) -> w*x) (zip weights points)


gradDesc :: [[Double]] -> [Double] -> Double -> Int -> [Double]
gradDesc x y α it = gradDesc' x y w0 α it
  where
    w0 = take (length' $ head x) [0.01,0.01..]

gradDesc' :: [[Double]] -> [Double] -> [Double] -> Double -> Int -> [Double]
gradDesc' x y w α it
  | convergiu = w
  | otherwise = gradDesc' x y w' α (it-1)
  where
    w'        = w .+. (α *. nabla)
    nabla     = mediaVetor $ map (\(yi, xi) -> yi *. xi) $ zip (y .-. y') x
    y'        = map (dotprod w) x
    convergiu = (erro' < 1e-6) || (it==0)
    erro'     = erro x y w
    
erro x y w = mean $ (y .-. y') .^ 2
  where
    y' = map (dotprod w) x    

mean :: [Double] -> Double
mean l = (sum l) / (length' l)

polyfeats :: Int -> [[Double]] -> [[Double]]
polyfeats k x = map (\xi -> poly xi !! k) x
  where
    poly x' = foldr f ([1] : repeat []) x'
    f x''   = scanl1 $ (++) . map (*x'')



rmse :: [Double] -> [Double] -> Double
rmse ytrue ypred = sqrt (s / n)
    where
        s = sum $ map (\(y, y') -> (y - y')**2 ) (zip ytrue ypred)
        n = fromIntegral $ length ytrue

----------------------------------------------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    let train = map fst dataset_train
    let test = map fst dataset_test
    let ytrue = map snd dataset_test
    let train_scaled = padroniza train
    let media_sigma = getMediaSigma train
    let test_scaled = transpose $ padronizaTestData media_sigma (transpose test)
    let test_scaled_p = zip test_scaled ytrue
    let train_scaled_p = zip train_scaled (map snd dataset_train)
    let ypred = knn1 train_scaled_p test_scaled_p
    putStr "Accuracy = "
    print $ acuraccy ytrue ypred
    putStr "Precision = "
    print $ precision 1 ytrue ypred
    putStr "Recall = "
    print $ recall 1 ytrue ypred
