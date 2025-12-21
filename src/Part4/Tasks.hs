{-# LANGUAGE InstanceSigs #-}

module Part4.Tasks where

import Util (notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a

infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
  reverse (reversed lst)
  where
    reversed REmpty = []
    reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = go REmpty
  where
    go acc [] = acc
    go acc (x : xs) = go (acc :< x) xs

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
  showsPrec :: Int -> ReverseList a -> ShowS
  showsPrec p xs = showString "[" . showContent xs . showString "]"
    where
      showContent REmpty = showString ""
      showContent (REmpty :< x) = shows x
      showContent (xs :< x) = showContent xs . showString "," . shows x

instance Eq (ReverseList a) where
  (==) = notImplementedYet
  (/=) = notImplementedYet

instance Semigroup (ReverseList a)

instance Monoid (ReverseList a)

instance Functor ReverseList

instance Applicative ReverseList

instance Monad ReverseList
