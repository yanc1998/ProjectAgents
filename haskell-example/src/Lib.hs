{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc,
    fact,
    aplay,
    sumlist,
    alguno,
    alguno2,
    multiplica,
    multiplica2,
    restos,
    restos2,
    cuadrados,
    longitudes,
    orden,
    pares,
    masde,
    collatz,
    collatzSeqLengt,
    cumplen,
    esPrimo,
    primos,
    productos,
    factores,
    adyacentes,
    remDups,
    takeUntil,
    longPro,
    apsiguales,
    tienenApp,
    Person (..),
  )
where

someFunc :: IO ()
someFunc = putStrLn "Hello world"

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

aplay :: (a -> b) -> a -> b
aplay f n = f n

sumlist :: (Num a) => [a] -> a
sumlist [] = 0
sumlist (x : xs) = x + sumlist xs

alguno :: [Bool] -> Bool
alguno [] = False
alguno (x : xs) = if x then True else alguno xs

alguno2 :: [Bool] -> Bool
alguno2 [] = False
alguno2 (x : xs) = x || alguno2 xs

multiplica :: (Num a) => [a] -> a
multiplica [] = 0
multiplica [x] = x
multiplica (x : xs) = x * multiplica xs

multiplica2 :: (Num a) => [a] -> a
multiplica2 [] = 0
multiplica2 n@(x : xs)
  | length n == 1 = x
  | otherwise = x * multiplica2 xs

restos :: Integer -> [Integer] -> [Integer]
restos _ [] = []
restos n (x : xs) = [mod x n] ++ restos n xs

restos2 :: Integer -> [Integer] -> [Integer]
restos2 n list@(x : xs) = if length list == 0 then [] else [mod x n] ++ restos n xs

cuadrados :: (Num a) => [a] -> [a]
cuadrados [] = []
cuadrados (x : xs) = [x ^ 2] ++ cuadrados xs

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x : xs) = [length x] ++ longitudes xs

orden :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
orden [] = []
orden (tupla@(x1, y1) : xs) = if x1 < 3 * y1 then [tupla] ++ orden xs else orden xs

pares :: [Integer] -> [Integer]
pares [] = []
pares (x : xs) = if x `mod` 2 == 0 then [x] ++ pares xs else pares xs

masde :: (Num a) => Int -> [[a]] -> [[a]]
masde _ [] = []
masde n (x : xs) = if length x > n then x : masde n xs else masde n xs

collatzt :: Integral a => a -> [a]
collatzt 1 = []
collatzt n = if even n then (n `div` 2) : collatzt (n `div` 2) else (3 * n + 1) : collatzt (3 * n + 1)

collatz :: Integral a => a -> [a]
collatz n = n : collatzt n

collatzSeqLengt :: Integral a => a -> Int -> [[a]]
collatzSeqLengt n m = filter (\x -> length x == m) [collatz x | x <- [1 .. n]]

cumplen :: (a -> Bool) -> [a] -> Bool
cumplen = all

esPrimo :: Int -> Bool
esPrimo n = not (any (\x -> n `mod` x == 0) [2 .. n `div` 2])

primos :: Int -> [Int]
primos n = filter (\x -> esPrimo x && x <= n) [2 .. n]

productos :: [Int] -> Int
productos = foldr (*) 1

primosdiv :: Int -> [Int]
primosdiv x = filter (\y -> x `mod` y == 0) (primos x)

factores2 :: Int -> [Int] -> [Int]
factores2 _ [] = []
factores2 valor p@(x : xs) = if valor `mod` x == 0 then x : factores2 (valor `div` x) p else factores2 valor xs

factores :: Int -> [Int]
factores x = (factores2 x . primosdiv) x

palindromo :: [Char] -> Bool
palindromo s = reverse s == s

longPro :: [[a]] -> Double
longPro xs = realToFrac (sumlist [length x | x <- xs]) / realToFrac (length xs)

adyacentes :: [a] -> [(a, a)]
adyacentes xs = [(xs !! i, xs !! (i + 1)) | i <- [0 .. length xs -2]]

remDups :: Eq a => [a] -> [a]
remDups xs = [xs !! i | i <- [0 .. length xs -1], i == length xs -1 || xs !! i /= xs !! (i + 1)]

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs) = if (not . p) x then x : takeUntil p xs else []

type Nombre = String

type Apellido = String

type Edad = Int

data Person = Person
  { nombre :: Nombre,
    apellido1 :: Apellido,
    apellido2 :: Apellido,
    edad :: Edad
  }
  deriving (Show)

apsiguales :: [Person] -> [Person]
apsiguales persons = [p | p@Person {apellido1 = ap1, apellido2 = ap2} <- persons, ap1 == ap2]

tienenApp :: [Person] -> (Apellido -> [Person])
tienenApp persons apellido = [p | p@Person {apellido1 = ap1, apellido2 = ap2} <- persons, ap1 == apellido || ap2 == apellido]

ninos :: [Person] -> [Person]
ninos persons = [p | p@Person {edad = e} <- persons, e < 11]

adolecentes :: [Person] -> [Person]
adolecentes persons = [p | p@Person {edad = e} <- persons, e >= 11 && e <= 17]

mayores :: [Person] -> [Person]
mayores persons = [p | p@Person {edad = e} <- persons, e >= 18]