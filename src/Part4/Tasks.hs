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

instance (Eq a) => Eq (ReverseList a) where
  (==) :: ReverseList a -> ReverseList a -> Bool
  (==) REmpty REmpty = True
  (==) REmpty _ = False
  (==) _ REmpty = False
  (==) (xs :< x) (ys :< y) = x == y && xs == ys

instance Semigroup (ReverseList a) where
  (<>) :: ReverseList a -> ReverseList a -> ReverseList a
  (<>) xs REmpty = xs
  (<>) xs (ys :< y) = (xs <> ys) :< y

instance Monoid (ReverseList a) where
  mempty :: ReverseList a
  mempty = REmpty

instance Functor ReverseList where
  fmap :: (a -> b) -> ReverseList a -> ReverseList b
  fmap _ REmpty = REmpty
  fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
  pure :: a -> ReverseList a
  pure x = REmpty :< x

  (<*>) :: ReverseList (a -> b) -> ReverseList a -> ReverseList b
  (<*>) REmpty _ = REmpty
  (<*>) _ REmpty = REmpty
  (<*>) (fs :< f) xs = (fs <*> xs) <> fmap f xs

instance Monad ReverseList where
  (>>=) :: ReverseList a -> (a -> ReverseList b) -> ReverseList b
  (>>=) REmpty _ = REmpty
  (>>=) (xs :< x) f = (xs >>= f) <> f x
