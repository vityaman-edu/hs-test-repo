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
myGCD x y = if mn == 0 then mx else myGCD mn (mx `mod` mn)
  where
    mn = min (abs x) (abs y)
    mx = max (abs x) (abs y)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = case () of
  _
    | year <= 0 -> False
    | month <= 0 -> False
    | 12 < month -> False
    | day <= 0 -> False
    | daysInMonth month (isLeap year) < day -> False
    | otherwise -> True
  where
    isLeap year =
      year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
    daysInMonth month is_leap =
      case month of
        2 -> if is_leap then 29 else 28
        x | x `elem` [4, 6, 9, 11] -> 30
        _ -> 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x e = product (replicate (fromInteger e) x)

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
