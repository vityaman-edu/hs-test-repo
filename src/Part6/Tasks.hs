{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Part6.Tasks where

import Data.Map
import qualified Data.Map as Map
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в
-- sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix m where
  matrixEye :: Int -> m
  matrixZero :: Int -> Int -> m

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  matrixEye :: Int -> Int
  matrixEye 1 = 1

  matrixZero :: Int -> Int -> Int
  matrixZero 1 1 = 0

instance Matrix [[Int]] where
  matrixEye :: Int -> [[Int]]
  matrixEye w = fmap row [0 .. w - 1]
    where
      row i = fmap column [0 .. w - 1]
        where
          column j = if j == i then 1 else 0

  matrixZero :: Int -> Int -> [[Int]]
  matrixZero w h = replicate h $ replicate w 0

instance Matrix (SparseMatrix Int) where
  matrixEye :: Int -> SparseMatrix Int
  matrixEye w = SparseMatrix w w (Map.fromList [((i, i), 1) | i <- [0 .. w - 1]])

  matrixZero :: Int -> Int -> SparseMatrix Int
  matrixZero w h = SparseMatrix w h Map.empty

-- Реализуйте следующие функции
-- Единичная матрица
eye :: (Matrix m) => Int -> m
eye = matrixEye

-- Матрица, заполненная нулями
zero :: (Matrix m) => Int -> Int -> m
zero = matrixZero

-- Перемножение матриц
multiplyMatrix :: (Matrix m) => m -> m -> m
multiplyMatrix = notImplementedYet -- TODO

-- Определитель матрицы
determinant :: (Matrix m) => m -> Int
determinant = notImplementedYet -- TODO
