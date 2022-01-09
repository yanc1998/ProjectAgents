module Elementos.Corral
  ( Corral (..),
    iscorralInPos,
  )
where

import Utils (isContain)

data Corral = Corral
  { valor :: [(Int, Int)]
  }
  deriving (Show)

iscorralInPos :: Corral -> (Int, Int) -> Bool
iscorralInPos = isContain . valor