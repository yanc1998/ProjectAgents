module Utils
  ( isContain,
    adyacentesPos,
    isValidPos,
    showIO,
    randomNumber,
    createSquare
  )
where

import GHC.IO (unsafePerformIO)
import System.Random (Random (randomRs), StdGen, getStdRandom, newStdGen, random, randomR)

randomNumber :: Int -> StdGen -> [Int]
randomNumber n = randomRs (0, n)

isContain :: (Eq a) => [a] -> a -> Bool
isContain [] _ = False
isContain (x : xs) value = (x == value) || isContain xs value

directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (-1, 0), (0, -1)]

directionsToSquare :: [(Int, Int)]
directionsToSquare = directions ++ [(1, -1), (-1, 1), (1, 1), (-1, -1)]

adyacentesPos :: (Int, Int) -> [(Int, Int)]
adyacentesPos (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

adyacentesPosToSquare :: (Int, Int) -> [(Int, Int)]
adyacentesPosToSquare (x, y) = [(x + dx, y + dy) | (dx, dy) <- directionsToSquare]

isValidPos :: (Int, Int) -> Int -> Int -> Bool
isValidPos (x, y) n m = x < n && y < m && x>=0 && y>=0 

createSquare :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int->Int-> [(Int, Int)]
createSquare [] visit _ _ _= visit
createSquare _ visit 0 _ _= visit
createSquare pila@(p : ps) visit c n m = if isContain visit p || not(isValidPos p n m ) then createSquare ps visit c n m else createSquare (ps ++ adyacentesPosToSquare p) (p : visit) (c -1) n m

showIO :: Show a => IO a -> IO String
showIO = fmap show