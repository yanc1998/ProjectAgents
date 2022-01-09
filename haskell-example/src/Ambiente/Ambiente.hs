module Ambiente.Ambiente
  ( test,
    test2,
    test3,
    testMovObst
  )
where

import Elementos.Corral (Corral (Corral), iscorralInPos)
import Elementos.Ninos (Ninos (Ninos), isninosInPos)
import Elementos.Obstaculo (Obstaculo (Obstaculo), isobstaculoInPos, movListObst)
import Elementos.Robot (Robot (Robot), isRobotInPos)
import Elementos.Suciedad (Suciedad (Suciedad), issuciedadInPos)
import Utils (adyacentesPos, isValidPos, randomNumber, randomPos2)

data Ambiente = Ambiente
  { ninos :: Ninos,
    robots :: Robot,
    corral :: Corral,
    suciedad :: Suciedad,
    obstaculos :: Obstaculo,
    dimetions :: (Int, Int)
  }
  deriving (Show)

isEmpty :: Ambiente -> (Int, Int) -> Bool
isEmpty ambiente@Ambiente {ninos = n, robots = r, obstaculos = o, suciedad = s, corral = c} pos = not (isninosInPos n pos || isRobotInPos r pos || isobstaculoInPos o pos || issuciedadInPos s pos || iscorralInPos c pos)

getAllObstaculosInDirections :: Ambiente -> (Int, Int) -> Int -> [(Int, Int)]
getAllObstaculosInDirections ambiente@Ambiente {dimetions = (n, m), obstaculos = o} pos dir = if isValidPos postoMov n m && not (isEmpty ambiente postoMov) && isobstaculoInPos o postoMov then pos : getAllObstaculosInDirections ambiente postoMov dir else [pos]
  where
    postoMov = adyacentesPos pos !! dir

movObstaculos :: Ambiente -> (Int, Int) -> Int -> Ambiente
movObstaculos ambiente@Ambiente {dimetions = (n, m), obstaculos = Obstaculo obs, suciedad = s, corral = c, robots = r, ninos = ni} pos dir = if isValidPos nextLast n m && isEmpty ambiente nextLast then Ambiente {dimetions = (n, m), suciedad = s, corral = c, robots = r, ninos = ni, obstaculos = Obstaculo (movListObst obs allObst dir)} else ambiente
  where
    allObst = getAllObstaculosInDirections ambiente pos dir
    len = length allObst
    lastObst = allObst !! (len -1)
    nextLast = adyacentesPos lastObst !! dir

test :: Ambiente -> Bool
test ambiente = isEmpty ambiente (2, 3)

test2 :: Bool
test2 = test Ambiente {ninos = Ninos [(2, 3)], robots = Robot [(1, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4)], dimetions = (5, 5)}

test3 :: [(Int, Int)]
test3 = let amb = Ambiente {ninos = Ninos [(2, 3)], robots = Robot [(1, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4), (1, 5)], dimetions = (5, 6)} in getAllObstaculosInDirections amb (1, 3) 0

testMovObst :: Ambiente
testMovObst = let amb = Ambiente {ninos = Ninos [(2, 3)], robots = Robot [(2, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4)], dimetions = (5, 6)} 
              in movObstaculos amb (1,3) 0  