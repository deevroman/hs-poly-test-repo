{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map hiding (drop, take, map)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    z :: Int -> Int -> mx
    e ::  Int -> mx
    multiply :: mx -> mx -> mx
    det :: mx -> Int

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    z _ _ = 0
    e _ = 1
    multiply = (*)
    det x = x

instance Matrix [[Int]] where
    z n m = [[0 | y <- [1..n]] | x <- [1..m]]
    e n = [[fromEnum (x == y) | y <- [1..n]] | x <- [1..n]]
    multiply a b = [[sum (zipWith (*) row col) | col <- transpose b] | row <- a]
      where
        transpose [] = []
        transpose ([] : _) = []
        transpose x = map head x : transpose (map tail x)
    det [[x]] = x
    det m = sum [((-1) ^ col) * head m !! col * det (minor m col) | col <- [0..length m - 1]]
      where
        minor m i = map (removeAt i) (tail m)
        removeAt i xs = take i xs ++ drop (i + 1) xs

instance Matrix (SparseMatrix Int) where
    z cols rows = SparseMatrix cols rows empty
    e n = SparseMatrix n n (fromList [((i, i), 1) | i <- [0..n-1]])
    multiply (SparseMatrix n m elems1) (SparseMatrix w2 h2 elems2) = SparseMatrix w2 m $ fromList
            [ ((row, col), v)
            | row <- [0..m-1], col <- [0..w2-1],
              let v = sum [findWithDefault 0 (row, k) elems1 * findWithDefault 0 (k, col) elems2 | k <- [0..n-1]],
              v /= 0]
    det (SparseMatrix n m elems)
        | n == 1 = findWithDefault 0 (0, 0) elems
        | otherwise = sum [((-1) ^ col) * findWithDefault 0 (0, col) elems * det (minorSparse col) | col <- [0..n-1]]
      where
        minorSparse col = SparseMatrix (n - 1) (m - 1) $ fromList
            [ ((row' - 1, col' - (if col' > col then 1 else 0)), v)
            | ((row', col'), v) <- toList elems, row' > 0, col' /= col ]

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = e
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = z
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = multiply

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = det