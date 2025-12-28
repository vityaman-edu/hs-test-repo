{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Part6.Tasks where

import Data.Either (fromRight)
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple (swap)
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в
-- sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

type MatrixResult a = Either String a

-- Определите класс типов "Матрица" с необходимыми (как вам кажется)
-- операциями, которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  type MatrixElement mx :: *

  matrixZero :: Int -> Int -> MatrixResult mx
  matrixHeight :: mx -> Int
  matrixWidth :: mx -> Int
  matrixGet' :: Int -> Int -> mx -> MatrixElement mx
  matrixPut' :: Int -> Int -> MatrixElement mx -> mx -> mx

  matrixProduct :: (Num (MatrixElement mx)) => mx -> mx -> MatrixResult mx
  matrixProduct = matrixProductNaive

  matrixTranspose :: mx -> MatrixResult mx
  matrixTranspose = matrixTransposeNaive

matrixGet :: (Matrix mx) => Int -> Int -> mx -> MatrixResult (MatrixElement mx)
matrixGet i j m = do
  _ <- matrixCheckRange i j m
  return $ matrixGet' i j m

matrixPut :: (Matrix mx) => Int -> Int -> MatrixElement mx -> mx -> MatrixResult mx
matrixPut i j e m = do
  _ <- matrixCheckRange i j m
  return $ matrixPut' i j e m

matrixCheckRange :: (Matrix mx) => Int -> Int -> mx -> MatrixResult ()
matrixCheckRange i j m
  | i < 0 || j < 0 = Left $ "Negative index " ++ show (i, j)
  | h < i || w < j = Left $ "Out of range index " ++ show (i, j)
  | otherwise = Right ()
  where
    h = matrixHeight m
    w = matrixWidth m

matrixSize :: (Matrix mx) => mx -> (Int, Int)
matrixSize m = (matrixHeight m, matrixWidth m)

matrixNew :: (Matrix mx) => Int -> Int -> (Int -> Int -> MatrixElement mx) -> MatrixResult mx
matrixNew h w e = do
  z <- matrixZero h w
  let is = [(i, j) | i <- [0 .. h - 1], j <- [0 .. w - 1]]
      put (i, j) = matrixPut' i j (e i j)
  return $ foldl (flip put) z is

matrixProductNaive :: (Matrix mx, Num (MatrixElement mx)) => mx -> mx -> MatrixResult mx
matrixProductNaive lhs rhs
  | n /= n1 =
      Left $
        "Product incompatible "
          ++ (show (m, n) ++ " and " ++ show (n1, p))
  | otherwise = matrixNew m p c
  where
    ((m, n), (n1, p)) = (matrixSize lhs, matrixSize rhs)
    a i j = matrixGet' i j lhs
    b i j = matrixGet' i j rhs
    c i j = sum $ fmap (\k -> a i k * b k j) [0 .. n - 1]

matrixTransposeNaive :: (Matrix mx) => mx -> MatrixResult mx
matrixTransposeNaive m = matrixNew w h e
  where
    (h, w) = matrixSize m
    e j i = matrixGet' i j m

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  type MatrixElement Int = Int

  matrixZero :: Int -> Int -> MatrixResult Int
  matrixZero 1 1 = Right 0
  matrixZero h w =
    Left $ "Matrix Int can be only (1, 1), " ++ ("not " ++ show (h, w))

  matrixHeight :: Int -> Int
  matrixHeight _ = 1

  matrixWidth :: Int -> Int
  matrixWidth _ = 1

  matrixGet' :: Int -> Int -> Int -> Int
  matrixGet' 0 0 m = m

  matrixPut' :: Int -> Int -> Int -> Int -> Int
  matrixPut' 0 0 e _ = e

instance (Num a) => Matrix [[a]] where
  type MatrixElement [[a]] = a

  matrixZero :: Int -> Int -> MatrixResult [[a]]
  matrixZero h w
    | h <= 0 || w <= 0 = Left $ "Non-positive size " ++ show (h, w)
    | otherwise = Right $ [[0 | j <- [1 .. w]] | i <- [1 .. h]]

  matrixHeight :: [[a]] -> Int
  matrixHeight = length

  matrixWidth :: [[a]] -> Int
  matrixWidth m
    | null lengths = 0
    | otherwise = maximum lengths
    where
      lengths = fmap length m

  matrixGet' :: Int -> Int -> [[a]] -> a
  matrixGet' 0 j (row : _) = fromMaybe 0 $ listToMaybe (drop j row)
  matrixGet' i j (_ : rows) = matrixGet' (i - 1) j rows

  matrixPut' :: Int -> Int -> a -> [[a]] -> [[a]]
  matrixPut' 0 j e (cols : rows) = replaceNth j e cols : rows
    where
      replaceNth i e [] = replicate i 0 ++ [e]
      replaceNth 0 e (x : xs) = e : xs
      replaceNth i e (x : xs) = x : replaceNth (i - 1) e xs
  matrixPut' i j e (cols : rows) = cols : matrixPut' (i - 1) j e rows

  matrixTranspose :: [[a]] -> MatrixResult [[a]]
  matrixTranspose m = Right $ transpose $ fmap (fill w) m
    where
      w = matrixWidth m
      fill w [] = replicate w 0
      fill w (x : xs) = x : fill (w - 1) xs

instance (Num a, Eq a) => Matrix (SparseMatrix a) where
  type MatrixElement (SparseMatrix a) = a

  matrixZero :: Int -> Int -> MatrixResult (SparseMatrix a)
  matrixZero h w
    | h <= 0 || w <= 0 = Left $ "Non-positive size " ++ show (h, w)
    | otherwise = Right $ SparseMatrix w h Map.empty

  matrixHeight :: SparseMatrix a -> Int
  matrixHeight = sparseMatrixHeight

  matrixWidth :: SparseMatrix a -> Int
  matrixWidth = sparseMatrixWidth

  matrixGet' :: Int -> Int -> SparseMatrix a -> MatrixElement (SparseMatrix a)
  matrixGet' i j = Map.findWithDefault 0 (i, j) . sparseMatrixElements

  matrixPut' :: Int -> Int -> MatrixElement (SparseMatrix a) -> SparseMatrix a -> SparseMatrix a
  matrixPut' i j e m = SparseMatrix w h $ put e es
    where
      h = matrixHeight m
      w = matrixWidth m
      es = sparseMatrixElements m
      put 0 = Map.delete (i, j)
      put e = Map.insert (i, j) e

  matrixTranspose :: SparseMatrix a -> MatrixResult (SparseMatrix a)
  matrixTranspose m = Right $ SparseMatrix h w es
    where
      h = matrixHeight m
      w = matrixWidth m
      es =
        foldr
          (\((i, j), x) m -> Map.insert (j, i) x m)
          Map.empty
          (Map.toList $ sparseMatrixElements m)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: (Matrix m, Num (MatrixElement m)) => Int -> m
eye w = case matrixNew w w e of
  Right m -> m
  Left m -> error m
  where
    e i j
      | i == j = 1
      | otherwise = 0

-- Матрица, заполненная нулями
zero :: (Matrix m) => Int -> Int -> m
zero w h = case matrixZero h w of
  Right m -> m
  Left m -> error m

-- Перемножение матриц
multiplyMatrix :: (Matrix m, Num (MatrixElement m)) => m -> m -> m
multiplyMatrix a b = case matrixProduct a b of
  Right m -> m
  Left m -> error m

-- Определитель матрицы
determinant :: (Matrix m) => m -> Int
determinant = notImplementedYet

fromLoL :: (Num a, Eq a) => [[a]] -> MatrixResult (SparseMatrix a)
fromLoL lol = matrixNew h w e
  where
    h = matrixHeight lol
    w = matrixWidth lol
    e i j = matrixGet' i j lol
