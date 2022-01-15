module Elementos.Ninos
  ( Ninos (..),
    isninosInPos,
    updateChildren2,
  )
where

import Utils (isContain)

data Ninos = Ninos
  { valor :: [(Int, Int)]
  }
  deriving (Show)

isninosInPos :: Ninos -> (Int, Int) -> Bool
isninosInPos = isContain . valor

updateChildren2 :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
updateChildren2 [] _ _ = []
updateChildren2 (x : xs)  pos1 pos2 = if pos1 == x then pos2 : xs else x:updateChildren2 xs pos1 pos2

