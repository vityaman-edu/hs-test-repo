module Part6.Tests where

import qualified Data.Map
import Part6.Tasks
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

unit_eye = do
  eye 1 @?= one
  eye 1 @?= [[one]]
  eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
  eye 2 @?= [[one, 0], [0, one]]
  eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])
  where
    one :: Int; one = 1

unit_zero = do
  zero 1 1 @?= zz
  zero 2 1 @?= [[zz, zz]]
  zero 2 2 @?= [[zz, zz], [zz, zz]]
  zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([] :: [((Int, Int), Int)]))
  where
    zz :: Int; zz = 0

-- Thanks, dmfrpro!
unit_multiplyMatrix = do
  let fromList2D lol = case fromLoL lol of
        Right m -> m
        Left m -> error m
      getElem m i j = matrixGet' i j m

  multiplyMatrix (2 :: Int) (3 :: Int) @?= 6

  let a = [[1, 2], [3, 4]] :: [[Int]]
  let b = [[5, 6], [7, 8]] :: [[Int]]
  multiplyMatrix a b @?= [[19, 22], [43, 50]]

  let c = [[1, 2, 3], [4, 5, 6]] :: [[Int]]
  let d = [[7, 8], [9, 10], [11, 12]] :: [[Int]]
  multiplyMatrix c d @?= [[58, 64], [139, 154]]

  multiplyMatrix a (eye 2 :: [[Int]]) @?= a
  multiplyMatrix (eye 2 :: [[Int]]) a @?= a

  multiplyMatrix a (zero 2 2 :: [[Int]]) @?= zero 2 2
  multiplyMatrix (zero 2 2 :: [[Int]]) a @?= zero 2 2

  let sparseA = fromList2D a :: SparseMatrix Int
  let sparseB = fromList2D b :: SparseMatrix Int
  let result = multiplyMatrix sparseA sparseB
  getElem result 0 0 @?= 19
  getElem result 0 1 @?= 22
  getElem result 1 0 @?= 43
  getElem result 1 1 @?= 50

  multiplyMatrix sparseA (eye 2 :: SparseMatrix Int) @?= sparseA

  let sparseMat = fromList2D [[1, 0, 2], [0, 3, 0], [4, 0, 5]] :: SparseMatrix Int
  let sparseMat2 = fromList2D [[1, 0, 0], [0, 2, 0], [0, 0, 3]] :: SparseMatrix Int
  let sparseResult = multiplyMatrix sparseMat sparseMat2
  getElem sparseResult 0 0 @?= 1
  getElem sparseResult 1 1 @?= 6
  getElem sparseResult 2 2 @?= 15

diffMatrixProduct :: [[Int]] -> [[Int]] -> [Maybe (SparseMatrix Int)]
diffMatrixProduct lhsLoL rhsLoL =
  let resLoL = do
        lol <- matrixProduct lhsLoL rhsLoL
        fromLoL lol
      resMap = do
        lhs <- fromLoL lhsLoL
        rhs <- fromLoL rhsLoL
        matrixProduct lhs rhs
   in [ eitherToMaybe resLoL,
        eitherToMaybe resMap
      ]

prop_matrixProduct :: [[Int]] -> [[Int]] -> Bool
prop_matrixProduct lhsLoL rhsLoL =
  let [resLoL, resMap] = diffMatrixProduct lhsLoL rhsLoL
   in resLoL == resMap

diffMatrixTranspose :: [[Int]] -> [Maybe (SparseMatrix Int)]
diffMatrixTranspose m =
  let smartLoL = do
        m <- matrixTranspose m
        fromLoL m
      naiveLoL = do
        m <- matrixTransposeNaive m
        fromLoL m
      naiveMap = do
        m <- fromLoL m
        matrixTransposeNaive m
   in fmap
        eitherToMaybe
        [ smartLoL,
          naiveLoL,
          naiveMap
        ]

prop_matrixTransposeLoL :: [[Int]] -> Bool
prop_matrixTransposeLoL m =
  let [smartLoL, naiveLoL, naiveMap] = diffMatrixTranspose m
   in smartLoL == naiveLoL && naiveLoL == naiveMap
