module Part1.Tasks where

import Util(notImplementedYet)

normalizeAngle :: Double -> Double
normalizeAngle angle = angle - (2 * pi) * fromIntegral (floor (angle / (2 * pi)))

-- синус числа (формула Тейлора)
factorialInv :: [Double]
factorialInv = scanl (/) 1 [1..]

sinCoeffs :: [Double]
sinCoeffs = 0 : 1 : 0 : -1 : sinCoeffs

powerSeries :: [Double] -> Double -> [Double]
powerSeries cs x = zipWith3 (\a b c -> a * b * c) cs powers factorialInv
   where powers = iterate (*x) 1

mySin :: Double -> Double
mySin = sum . take 30 . powerSeries sinCoeffs . normalizeAngle

-- косинус числа (формула Тейлора)
cosinCoeffs :: [Double]
cosinCoeffs = 1 : 0 : -1 : 0 : cosinCoeffs

myCos :: Double -> Double
myCos = sum . take 30 . powerSeries cosinCoeffs . normalizeAngle


-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b
  | a == 0 = abs b
  | b == 0 = abs a
  | otherwise = myGCD b (mod a b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isYearLeap :: Integer -> Bool
isYearLeap year
            | year `mod` 400 == 0 = True
            | year `mod` 100 == 0 = False
            | year `mod` 4 == 0 = True
            | otherwise = False

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | month < 1 || month > 12 = False
    | day < 1 = False
    | otherwise = day <= daysInMonth month year
  where
    daysInMonth m y = case m of
        2 -> if isYearLeap y then 29 else 28
        4 -> 30
        6 -> 30
        9 -> 30
        11 -> 30
        _ -> 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x y = x * myPow x (y - 1) 

-- является ли данное число простым?
isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper n divisor
    | divisor < 2          = True
    | n `mod` divisor == 0 = False
    | otherwise            = isPrimeHelper n (divisor - 1)

isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | otherwise = isPrimeHelper n (floor . sqrt $ fromIntegral n)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea [] = 0
shapeArea [_] = 0
shapeArea points = abs (gaussFormula (points ++ [head points])) / 2
  where
    gaussFormula [] = 0
    gaussFormula [_] = 0
    gaussFormula ((x1, y1):(x2, y2):ps) = (x1 * y2 - y1 * x2) + gaussFormula ((x2, y2):ps)


-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | isInvalid a b c = -1
    | isRightTriangle a b c = 2
    | isAcuteTriangle a b c = 1
    | otherwise = 0
  where
    isTriangle x y z = x + y > z && x + z > y && y + z > x

    isInvalid x y z = x <= 0 || y <= 0 || z <= 0 || (not (isTriangle x y z))

    isRightTriangle x y z = 
        (x * x + y * y == z * z) || 
        (x * x + z * z == y * y) || 
        (y * y + z * z == x * x)

    isAcuteTriangle x y z = 
        (x * x < y * y + z * z) && 
        (y * y < x * x + z * z) && 
        (z * z < x * x + y * y)
