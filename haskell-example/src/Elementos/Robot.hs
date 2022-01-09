module Elementos.Robot
  ( Robot (..),
    isRobotInPos,
  )
where

import Utils (isContain)

data Robot = Robot
  { valor :: [(Int, Int)]
  }
  deriving (Show)

isRobotInPos :: Robot -> (Int, Int) -> Bool
isRobotInPos = isContain . valor
