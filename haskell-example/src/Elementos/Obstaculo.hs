module Elementos.Obstaculo
  ( Obstaculo (..),
    isobstaculoInPos,
    movListObst
  )
where

import Utils (isContain, adyacentesPosToSquare)

data Obstaculo = Obstaculo
  { valor :: [(Int, Int)]
  }
  deriving (Show)

isobstaculoInPos :: Obstaculo -> (Int, Int) -> Bool
isobstaculoInPos = isContain . valor

movListObst :: [(Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
movListObst obs [] _ = obs
movListObst [] _ _ = []
movListObst (x : xs) toMov dir = if isContain toMov x then adyacentesPosToSquare x !! dir : movListObst xs toMov dir else x : movListObst xs toMov dir