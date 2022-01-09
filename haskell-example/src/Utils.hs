module Utils
  ( isContain,
    randomNumber,
    randomPos,
    randomPos2,
    adyacentesPos,
    isValidPos,
    createSquare,
    showIO,
  )
where

import GHC.IO (unsafePerformIO)
import System.Random (Random (randomR), getStdRandom)

isContain :: (Eq a) => [a] -> a -> Bool
isContain [] _ = False
isContain (x : xs) value = (x == value) || isContain xs value

randomNumber :: Int -> IO Int
randomNumber n = getStdRandom $ randomR (0, n)

randomPos :: Int -> Int -> (Int, Int)
randomPos n m = (n, m)

randomPos2 :: Int -> Int -> IO (Int, Int)
randomPos2 n m = do
  a <- randomNumber n
  b <- randomNumber m
  return $ randomPos a b


directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (-1, 0), (0, -1)]

adyacentesPos :: (Int, Int) -> [(Int, Int)]
adyacentesPos (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

isValidPos :: (Int, Int) -> Int -> Int -> Bool
isValidPos (x, y) n m = x < n && y < m

createSquare :: (Int, Int) -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
createSquare _ _ _ 0 _ = []
createSquare (x, y) n m cant visit = if not (isContain visit (x, y)) && isValidPos (x, y) n m then (x, y) : callAdy n m (cant-1) (visit ++ [(x, y)]) (adyacentesPos (x, y)) else []

callAdy :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
callAdy n m cant visit [] = []
callAdy n m cant visit (a : ady) = (createSquare a n m cant visit) ++ ( callAdy n m cant visit ady)

showIO :: Show a => IO a -> IO String
showIO = fmap show