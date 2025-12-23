{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module Part5.Tasks where

import Util (notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f ini [] = ini
myFoldl f ini (x : xs) = myFoldl f (f ini x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f ini [] = ini
myFoldr f ini (x : xs) = f x (myFoldr f ini xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (mappend f) []
  where
    mappend f y xs = f y : xs

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f xs = myConcat $ myMap f xs

myReverse :: [a] -> [a]
myReverse = notImplementedYet

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = notImplementedYet

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = notImplementedYet
