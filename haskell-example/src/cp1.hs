module Cp1
  ( fact,
    aplay,
    alguno,
    sumlist,
    alguno2,
    multiplica,
    multiplica2,
    restos2,
    restos,
    cuadrados,
    longitudes,
    orden,
    pares,
    masde,
  )
where

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