module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show REmpty = "[]"
    show lst = "[" ++ showHelper lst ++ "]"
      where
        showHelper REmpty = ""
        showHelper (REmpty :< x) = show x
        showHelper (xs :< x) = showHelper xs  ++ "," ++ show x

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) (xs :< x) (ys :< y) = xs == ys && x == y
    (==) _ _ = False
    
instance Semigroup (ReverseList a) where
  (<>) xs REmpty = xs
  (<>) xs (ys :< y) = (xs <> ys) :< y
  
instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap _ REmpty = REmpty
  fmap f (xs :< x) = fmap f xs :< f x
  
instance Applicative ReverseList where
  pure = (REmpty :<)
  (<*>) REmpty _ = REmpty
  (<*>) (xs :< x) lst = (xs <*> lst) <> fmap x lst
  
instance Monad ReverseList where
  return = pure
  (>>=) REmpty _ = REmpty
  (>>=) (xs :< x) f = (xs >>= f) <> f x