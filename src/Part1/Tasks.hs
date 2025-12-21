module Part1.Tasks where

import Data.Fixed (mod')
import Util (notImplementedYet)

factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = maclaurin (normalized x) 100
  where
    maclaurin :: Double -> Integer -> Double
    maclaurin x n = sum (fmap term [0 .. n])
      where
        term n = (((-1) ^ n) * (x ^ (2 * n + 1))) / fromIntegral (factorial (2 * n + 1))
    normalized :: Double -> Double
    normalized x = x `mod'` (2 * pi)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = maclaurin (normalized x) 100
  where
    maclaurin :: Double -> Integer -> Double
    maclaurin x n = sum (fmap term [0 .. n])
      where
        term n = (-1) ^^ n * x ^^ (2 * n) / fromIntegral (factorial (2 * n))
    normalized :: Double -> Double
    normalized x = x `mod'` (2 * pi)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD = notImplementedYet

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect = notImplementedYet

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow = notImplementedYet

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = notImplementedYet

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
-- shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
