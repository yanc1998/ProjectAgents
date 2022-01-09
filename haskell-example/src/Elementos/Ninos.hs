module Elementos.Ninos
  ( Ninos (..),
    isninosInPos,
  )
where

import Utils (isContain)

data Ninos = Ninos
  { valor :: [(Int, Int)]
  }
  deriving (Show)

isninosInPos :: Ninos -> (Int, Int) -> Bool
isninosInPos = isContain . valor
