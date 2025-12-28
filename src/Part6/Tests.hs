module Part6.Tests where

import qualified Data.Map
import Part6.Tasks
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

unit_eye :: IO ()
unit_eye = do
  eye 1 @?= one
  eye 1 @?= [[one]]
  eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
  eye 2 @?= [[one, 0], [0, one]]
  eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])
  where
    one :: Int; one = 1

unit_zero :: IO ()
unit_zero = do
  zero 1 1 @?= zz
  zero 2 1 @?= [[zz, zz]]
  zero 2 2 @?= [[zz, zz], [zz, zz]]
  zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([] :: [((Int, Int), Int)]))
  where
    zz :: Int; zz = 0

-- Thanks, dmfrpro!
unit_multiplyMatrix :: IO ()
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
  let smartLoL = do
        lol <- matrixProduct lhsLoL rhsLoL
        fromLoL lol
      naiveLoL = do
        lol <- matrixProductNaive lhsLoL rhsLoL
        fromLoL lol
      smartMap = do
        lhs <- fromLoL lhsLoL
        rhs <- fromLoL rhsLoL
        matrixProduct lhs rhs
      naiveMap = do
        lhs <- fromLoL lhsLoL
        rhs <- fromLoL rhsLoL
        matrixProductNaive lhs rhs
   in fmap
        eitherToMaybe
        [ smartLoL,
          naiveLoL,
          smartMap,
          naiveMap
        ]

prop_matrixProduct :: [[Int]] -> [[Int]] -> Bool
prop_matrixProduct lhsLoL rhsLoL =
  let [smartLoL, naiveLoL, smartMap, naiveMap] = diffMatrixProduct lhsLoL rhsLoL
   in smartLoL == naiveLoL
        && naiveLoL == smartMap
        && smartMap == naiveMap

diffMatrixTranspose :: [[Int]] -> [Maybe (SparseMatrix Int)]
diffMatrixTranspose m =
  let smartLoL = do
        m <- matrixTranspose m
        fromLoL m
      naiveLoL = do
        m <- matrixTransposeNaive m
        fromLoL m
      smartMap = do
        m <- fromLoL m
        matrixTranspose m
      naiveMap = do
        m <- fromLoL m
        matrixTransposeNaive m
   in fmap
        eitherToMaybe
        [ smartLoL,
          naiveLoL,
          smartMap,
          naiveMap
        ]

prop_matrixTransposeLoL :: [[Int]] -> Bool
prop_matrixTransposeLoL m =
  let [smartLoL, naiveLoL, smartMap, naiveMap] = diffMatrixTranspose m
   in smartLoL == naiveLoL
        && naiveLoL == smartMap
        && smartMap == naiveMap

-- Thanks, dmfrpro!
unit_determinant :: IO ()
unit_determinant = do
  let fromList2D lol = case fromLoL lol of
        Right m -> m
        Left m -> error m
      getElem m i j = matrixGet' i j m

  determinant (5 :: Int) @?= 5
  determinant (0 :: Int) @?= 0
  determinant ((-3) :: Int) @?= -3

  determinant ([[7]] :: [[Int]]) @?= 7

  determinant ([[1, 2], [3, 4]] :: [[Int]]) @?= -2
  determinant ([[5, 6], [7, 8]] :: [[Int]]) @?= -2

  determinant ([[2, 5, 3], [1, -2, -1], [1, 3, 4]] :: [[Int]]) @?= -20

  determinant (eye 1 :: [[Int]]) @?= 1
  determinant (eye 2 :: [[Int]]) @?= 1
  determinant (eye 3 :: [[Int]]) @?= 1

  determinant (zero 2 2 :: [[Int]]) @?= 0
  determinant (zero 3 3 :: [[Int]]) @?= 0

  let sparse2x2 = fromList2D [[1, 2], [3, 4]] :: SparseMatrix Int
  determinant sparse2x2 @?= -2

  let sparse3x3 = fromList2D [[2, 5, 3], [1, -2, -1], [1, 3, 4]] :: SparseMatrix Int
  determinant sparse3x3 @?= -20

  determinant (eye 2 :: SparseMatrix Int) @?= 1

  determinant (zero 3 3 :: SparseMatrix Int) @?= 0

  let diagSparse = fromList2D [[2, 0, 0], [0, 3, 0], [0, 0, 4]] :: SparseMatrix Int
  determinant diagSparse @?= 24
