{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Part6.Tasks where

import Control.Monad (foldM)
import Data.Either (fromRight)
import Data.Map
import Data.Maybe (fromMaybe, listToMaybe)
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в
-- sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

class Field a where
  fzero :: a
  fone :: a

instance Field Int where
  fzero :: Int
  fzero = 0

  fone :: Int
  fone = 1

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix m where
  type MatrixElement m :: *

  matrixZero :: Int -> Int -> Either String m
  matrixWidth :: m -> Int
  matrixHeight :: m -> Int

  -- FIXME: Take the matrix as a second argument
  matrixGet :: m -> Int -> Int -> Either String (MatrixElement m)
  matrixPut :: m -> Int -> Int -> MatrixElement m -> Either String m

  -- Writting code nobody can read
  (*) :: (Num (MatrixElement m)) => m -> m -> Either String m
  (*) lhs rhs
    | n /= n1 =
        Left $
          "Can not compute product of matricies "
            ++ (show (m, n) ++ " and " ++ show (n1, p))
    where
      ((m, n), (n1, p)) = (matrixSize lhs, matrixSize rhs)
  (*) lhs rhs =
    let ((m, n), (_, p)) = (matrixSize lhs, matrixSize rhs)
        b i j = fromRight (error ":(") (matrixGet rhs i j)
        a i j = fromRight (error ":(") (matrixGet lhs i j)
        c i j = sum $ fmap (\k -> a i k Prelude.* b k j) [0 .. n - 1]
     in do
          zero <- matrixZero m p
          foldM
            (\m (i, j) -> matrixPut m i j (c i j))
            zero
            [(i, j) | i <- [0 .. m - 1], j <- [0 .. p - 1]]

matrixSize :: (Matrix m) => m -> (Int, Int)
matrixSize m = (matrixHeight m, matrixWidth m)

matrixBadSize :: String -> (Int, Int) -> String
matrixBadSize matrix size =
  matrix ++ " does not support size " ++ show size

matrixOutOfRange :: String -> (Int, Int) -> String
matrixOutOfRange matrix index =
  "index " ++ show index ++ " is out of range on " ++ matrix

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше

instance Matrix Int where
  type MatrixElement Int = Int

  matrixZero :: Int -> Int -> Either String Int
  matrixZero 1 1 = Right 0
  matrixZero h w = Left $ matrixBadSize "Matrix Int" (h, w)

  matrixHeight :: Int -> Int
  matrixHeight _ = 1

  matrixWidth :: Int -> Int
  matrixWidth _ = 1

  matrixGet :: Int -> Int -> Int -> Either String Int
  matrixGet m 0 0 = Right m
  matrixGet m i j =
    Left $ matrixOutOfRange ("Matrix Int [" ++ show m ++ "]") (i, j)

  matrixPut :: Int -> Int -> Int -> Int -> Either String Int
  matrixPut _ 0 0 e = Right e
  matrixPut m i j _ =
    Left $ matrixOutOfRange ("Matrix Int [" ++ show m ++ "]") (i, j)

instance (Field a) => Matrix [[a]] where
  type MatrixElement [[a]] = a

  matrixZero :: Int -> Int -> Either String [[a]]
  matrixZero h w | h <= 0 || w <= 0 = Left $ matrixBadSize "Matrix [[a]]" (h, w)
  matrixZero h w = Right [[fzero | _ <- [1 .. w]] | _ <- [1 .. h]]

  matrixHeight :: [[a]] -> Int
  matrixHeight = length

  matrixWidth :: [[a]] -> Int
  matrixWidth = Prelude.foldl maxLength 0
    where
      maxLength :: Int -> [a] -> Int
      maxLength len row = max len $ length row

  matrixGet :: [[a]] -> Int -> Int -> Either String a
  matrixGet m i j
    | (i < 0) || (j < 0) || (h <= i) || (w <= j) =
        Left $ matrixOutOfRange ("Matrix [[a]] " ++ show (h, w)) (i, j)
    where
      h = matrixHeight m
      w = matrixWidth m
  matrixGet m i j = Right $ fromMaybe fzero column
    where
      row = m !! i -- safe as `matrixHeight` definition
      column = safeGet j row
      safeGet n xs = listToMaybe (Prelude.drop n xs)

  matrixPut :: [[a]] -> Int -> Int -> a -> Either String [[a]]
  matrixPut m i j _
    | (i < 0) || (j < 0) || (h <= i) || (w <= j) =
        Left $ matrixOutOfRange ("Matrix [[a]] " ++ show (h, w)) (i, j)
    where
      h = matrixHeight m
      w = matrixWidth m
  matrixPut (row : rows) 0 j e = Right $ replaceNth j e row : rows
    where
      replaceNth :: (Field a) => Int -> a -> [a] -> [a]
      replaceNth n newVal xs
        | n < 0 = error ";(" -- safe as index check
        | n < length xs =
            let (ys, zs) = Prelude.splitAt n xs
             in ys ++ [newVal] ++ Prelude.drop 1 zs
        | otherwise =
            xs ++ replicate (n - length xs) fzero ++ [newVal]
  matrixPut (row : rows) i j e = do
    tail <- matrixPut rows (i - 1) j e
    return $ row : tail

instance (Field a) => Matrix (SparseMatrix a) where
  type MatrixElement (SparseMatrix a) = a

  matrixZero :: Int -> Int -> Either String (SparseMatrix a)
  matrixZero h w | h <= 0 || w <= 0 = Left $ matrixBadSize "Matrix SparseMatrix" (h, w)
  matrixZero h w = Right $ SparseMatrix h w empty

  matrixWidth :: SparseMatrix a -> Int
  matrixWidth = sparseMatrixWidth

  matrixHeight :: SparseMatrix a -> Int
  matrixHeight = sparseMatrixHeight

  matrixGet :: SparseMatrix a -> Int -> Int -> Either String a
  matrixGet m i j
    | (i < 0) || (j < 0) || (h <= i) || (w <= j) =
        Left $ matrixOutOfRange ("Matrix SparseMatrix " ++ show (h, w)) (i, j)
    where
      h = matrixHeight m
      w = matrixWidth m
  matrixGet m i j = Right $ findWithDefault fzero (i, j) (sparseMatrixElements m)

  matrixPut :: SparseMatrix a -> Int -> Int -> a -> Either String (SparseMatrix a)
  matrixPut m i j _
    | (i < 0) || (j < 0) || (h <= i) || (w <= j) =
        Left $ matrixOutOfRange ("Matrix SparseMatrix " ++ show (h, w)) (i, j)
    where
      h = matrixHeight m
      w = matrixWidth m
  matrixPut m i j e = Right $ SparseMatrix w h (insert (i, j) e xs)
    where
      h = matrixHeight m
      w = matrixWidth m
      xs = sparseMatrixElements m

-- Реализуйте следующие функции
-- Единичная матрица
eye :: (Matrix m, Field (MatrixElement m)) => Int -> m
eye w =
  Prelude.foldl (\m x -> unwrap $ matrixPut m x x fone) z [0 .. w - 1]
  where
    z = zero w w
    unwrap = fromRight (error ";(")

-- Матрица, заполненная нулями
zero :: (Matrix m) => Int -> Int -> m
zero w h = case matrixZero h w of
  Right m -> m
  Left message -> error message

-- Перемножение матриц
multiplyMatrix :: (Matrix m) => m -> m -> m
multiplyMatrix = notImplementedYet -- TODO

-- Определитель матрицы
determinant :: (Matrix m) => m -> Int
determinant = notImplementedYet -- TODO
