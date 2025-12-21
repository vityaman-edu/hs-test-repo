module Part2.Tasks where

import Util (notImplementedYet)
import Data.Semigroup (Min(Min))

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term
  = IntConstant {intValue :: Int} -- числовая константа
  | Variable {varName :: String} -- переменная
  | BinaryTerm {op :: BinaryOp, lhv :: Term, rhv :: Term} -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 6 |+|

infixl 6 |-|

infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
  case expression of
    Variable name | name == varName -> replacement
    BinaryTerm op lhv rhv ->
      BinaryTerm
        op
        (replaceVar varName replacement lhv)
        (replaceVar varName replacement rhv)
    _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op lhv rhv) = case (op, lhs, rhs) of
  (Plus, IntConstant lhs, IntConstant rhs) -> IntConstant $ lhs + rhs
  (Minus, IntConstant lhs, IntConstant rhs) -> IntConstant $ lhs - rhs
  (Times, IntConstant lhs, IntConstant rhs) -> IntConstant $ lhs * rhs
  _ -> BinaryTerm op lhs rhs
  where
    lhs = evaluate lhv
    rhs = evaluate rhv
evaluate x = x
