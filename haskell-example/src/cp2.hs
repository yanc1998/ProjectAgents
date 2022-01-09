module Cp2
  ( collatz,
    collatzSeqLengt,
    cumplen,
    esPrimo,
    primos,
    productos,
    factores,
    palindromo,
    longPro,
    adyacentes,
    remDups,
    takeUntil,
    Person (..),
    apsiguales,
    tienenApp,
    ninos,
    adolecentes,
    mayores,
  )
where

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
longPro xs = realToFrac (sum [length x | x <- xs]) / realToFrac (length xs)

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