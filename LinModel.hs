module LinModel(
    Model,
    DataSet,
    model,
    randomModel,
    dataSet,
    apply,
    optimalModel,
    gradientDescent
) where 

import System.Random
import qualified Numeric.LinearAlgebra as Lin
import Data.List
import Data.Bifunctor
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as Vec
import Data.Either

data Model = Model {weights :: Lin.Vector Double} deriving Show

--create a linear model from ghci
model :: [Double] -> Double -> Model
model weights disp = Model $ Lin.vector (disp : weights)

--create a random model
randomModel :: Int -> Double -> IO Model
randomModel featuresNum dist = do
    gen <- getStdGen
    let (disp:randomList) = map ((2*dist*) . (subtract 0.5)) . take (featuresNum+1) $ randoms gen
    return $ model randomList disp

data DataSet = DataSet {features :: Lin.Matrix Double, results :: Lin.Vector Double} deriving Show

--create a data set from ghci
dataSet :: [[Double]] -> [Double] -> DataSet
dataSet inputData result = DataSet inputMat expectVec
    where
    inputMat = Lin.fromLists $ map (1:) inputData
    expectVec = Lin.vector result


--apply the model to the data set
apply :: Model -> DataSet -> [Double]
apply model dataSet = Lin.toList resultVec
    where
    resultVec = weights model Lin.<# features dataSet


--create the optimal model for the data set
optimalModel :: DataSet -> Model
optimalModel dataSet = Model (mat1 Lin.#> mat2)
    where
    mat1 = Lin.inv $ Lin.tr (features dataSet) Lin.<> features dataSet
    mat2 = Lin.tr (features dataSet) Lin.#> results dataSet


--simple gradient descent
gradientDescent :: Model -> DataSet -> Int -> Lin.Vector Double -> Model
gradientDescent model dataSet iter alfa = church iter descentStep model
    where
    --single descent iteration
    descentStep :: Model -> Model
    descentStep model' = Model $ weights model' - alfa * gradient
        where
        gradient = 2 * trData Lin.#> err / fromIntegral objsNum
        err = features dataSet Lin.#> weights model' - results dataSet

    objsNum = Lin.rows $ features dataSet
    trData = Lin.tr $ features dataSet

--supporting functions:

--Church numerals (from combinatory logic)
church :: Int -> (a -> a) -> a -> a
church n f x 
    | n <= 0 = x
    | otherwise = let x' = f x in x' `seq` church (n-1) f x'