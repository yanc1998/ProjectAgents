module Utils
  ( isContain,
    adyacentesPos,
    isValidPos,
    createSquare,
    showIO,
    randomDirections,
    randomDirectionToNum,
    randomNumber,
    testRandom,Directions(..)
  )
where

import GHC.IO (unsafePerformIO)
import System.Random (Random (randomRs),random,getStdRandom,randomR,StdGen, newStdGen)

type Cell = (Int, Int)

data Directions = N | S | E | W deriving (Show, Read, Enum, Bounded)

instance Random Directions where
    random gen =
        let 
            (r, ngen) = random gen
            dir = toEnum (mod r 4)
        in (dir, ngen)

    randomR (d1, d2) gen =
        let 
            (r, ngen) = randomR (fromEnum d1, fromEnum d2) gen
            dir = toEnum (mod r 4)
        in (dir, ngen)

randomDirections :: StdGen -> [Directions]
randomDirections = randomRs (N, W)

randomNumber :: Int -> StdGen -> [Int]
randomNumber n = randomRs (0, n)  

randomDirectionToNum :: Directions -> Int
randomDirectionToNum d = case d of 
  N -> 0
  S -> 1
  E -> 2
  W -> 3

testRandom :: Directions -> IO ()
testRandom d = case d of 
  N -> print "N"
  S -> print "S"
  E -> print  "E"
  W -> print "W"

isContain :: (Eq a) => [a] -> a -> Bool
isContain [] _ = False
isContain (x : xs) value = (x == value) || isContain xs value



directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (-1, 0), (0, -1)]

adyacentesPos :: (Int, Int) -> [(Int, Int)]
adyacentesPos (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

isValidPos :: (Int, Int) -> Int -> Int -> Bool
isValidPos (x, y) n m = x < n && y < m

createSquare :: (Int, Int) -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
createSquare _ _ _ 0 _ = []
createSquare (x, y) n m cant visit = if not (isContain visit (x, y)) && isValidPos (x, y) n m then (x, y) : callAdy n m (cant -1) (visit ++ [(x, y)]) (adyacentesPos (x, y)) else []

callAdy :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
callAdy n m cant visit [] = []
callAdy n m cant visit (a : ady) = (createSquare a n m cant visit) ++ (callAdy n m cant visit ady)

showIO :: Show a => IO a -> IO String
showIO = fmap show