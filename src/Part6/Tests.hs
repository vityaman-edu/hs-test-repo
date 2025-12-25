module Part6.Tests where

import qualified Data.Map
import Part6.Tasks
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Util (notImplementedYet)

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

unit_product_ll_2x2 = do
  let m2x2a =
        [ [1, 2],
          [3, 4]
        ] ::
          [[Int]]

  let m2x2b =
        [ [5, 6],
          [7, 8]
        ] ::
          [[Int]]

  let expected1 =
        [ [19, 22],
          [43, 50]
        ] ::
          [[Int]]

  multiplyMatrix m2x2a m2x2b @?= expected1

  let m2x3 =
        [ [1, 2, 3],
          [4, 5, 6]
        ] ::
          [[Int]]

  let m3x2 =
        [ [7, 8],
          [9, 10],
          [11, 12]
        ] ::
          [[Int]]

  let expected2 =
        [ [58, 64],
          [139, 154]
        ] ::
          [[Int]]

  multiplyMatrix m2x3 m3x2 @?= expected2

  let id2 = eye 2 :: [[Int]]
  multiplyMatrix m2x2a id2 @?= m2x2a
  multiplyMatrix id2 m2x2a @?= m2x2a

  let z2 = zero 2 2 :: [[Int]]
  multiplyMatrix m2x2a z2 @?= z2
  multiplyMatrix z2 m2x2a @?= z2

  let rowVec = [[1, 2, 3]] :: [[Int]]
  let colVec = [[4], [5], [6]] :: [[Int]]
  let expected3 = [[32]] :: [[Int]]
  multiplyMatrix rowVec colVec @?= expected3
