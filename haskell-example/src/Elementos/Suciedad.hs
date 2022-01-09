module Elementos.Suciedad
  ( Suciedad (..),
    issuciedadInPos,
  )
where

import Utils (isContain)

data Suciedad = Suciedad
  { valor :: [(Int, Int)]
  }
  deriving (Show)

issuciedadInPos :: Suciedad -> (Int, Int) -> Bool
issuciedadInPos = isContain . valor