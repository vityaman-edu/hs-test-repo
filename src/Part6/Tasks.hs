{-# LANGUAGE FlexibleInstances #-}

module Part6.Tasks where

import Data.Map
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в
-- sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется)
-- операциями, которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int

instance Matrix [[Int]]

instance Matrix (SparseMatrix Int)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: (Matrix m) => Int -> m
eye w = notImplementedYet

-- Матрица, заполненная нулями
zero :: (Matrix m) => Int -> Int -> m
zero w h = notImplementedYet

-- Перемножение матриц
multiplyMatrix :: (Matrix m) => m -> m -> m
multiplyMatrix = notImplementedYet

-- Определитель матрицы
determinant :: (Matrix m) => m -> Int
determinant = notImplementedYet
