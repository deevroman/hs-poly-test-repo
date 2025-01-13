module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
                                             IntConstant _ -> expression
                                             BinaryTerm op lhv rhv ->
                                                   let newLhv = replaceVar varName replacement lhv
                                                       newRhv = replaceVar varName replacement rhv
                                                   in BinaryTerm op newLhv newRhv
                                             Variable name | name == varName -> replacement
                                                           | otherwise       -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant x) = IntConstant x
evaluate (Variable var) = Variable var
evaluate (BinaryTerm op lhv rhv) =
  case (op, evaluate lhv, evaluate rhv) of
    (Plus, IntConstant l, IntConstant r)  -> IntConstant (l + r)
    (Minus, IntConstant l, IntConstant r) -> IntConstant (l - r)
    (Times, IntConstant l, IntConstant r) -> IntConstant (l * r)
    _ -> BinaryTerm op (evaluate lhv) (evaluate rhv)