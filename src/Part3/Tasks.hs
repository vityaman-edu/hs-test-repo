module Part3.Tasks where

import Data.Foldable (find, maximumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Util (notImplementedYet)
import Data.Maybe (fromMaybe)

-- Функция finc принимает на вход функцию f и число n
-- и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = fmap f [n ..]

-- Функция ff принимает на вход функцию f и элемент x
-- и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру*
-- в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq xs = fst $ maximumBy (comparing snd) $ freq $ concatMap digits xs
  where
    digits :: Int -> [Int]
    digits x = digits' $ abs x
    digits' x = (x `mod` 10) : digits'' (x `div` 10)
    digits'' x = if x == 0 then [] else digits' x

    freq :: (Eq a) => [a] -> [(a, Int)]
    freq = foldl go []
      where
        go count x = case lookup x count of
          Nothing -> (x, 1) : count
          Just n -> (x, n + 1) : filter ((/= x) . fst) count

-- Дан список lst. Вернуть список элементов из lst без повторений,
-- порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и
-- каждому возможному значению результата применения F к элементам
-- Lst ставит в соответствие список элементов Lst, приводящих к
-- этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f [] = []
grokBy f (x : xs) =
  let fx = f x
      tail = grokBy f xs
      (k, ys) = fromMaybe (fx, []) $ find (\(k, _) -> k == fx) tail
   in (k, x : ys) : filter (\(k, _) -> k /= fx) tail
