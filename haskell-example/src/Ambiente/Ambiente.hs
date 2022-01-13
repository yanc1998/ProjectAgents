module Ambiente.Ambiente
  ( test,
    test2,
    test3,
    testMovObst,
    testMOveOneChildren,
  )
where

import Elementos.Corral (Corral (Corral), iscorralInPos)
import Elementos.Ninos (Ninos (Ninos), isninosInPos, updateChildren)
import Elementos.Obstaculo (Obstaculo (Obstaculo), isobstaculoInPos, movListObst)
import Elementos.Robot (Robot (Robot), isRobotInPos)
import Elementos.Suciedad (Suciedad (Suciedad), issuciedadInPos)
import System.Random (StdGen)
import Utils (Directions, adyacentesPos, isValidPos, randomDirectionToNum, randomDirections, randomNumber)

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
movObstaculos ambiente@Ambiente {dimetions = (n, m), obstaculos = Obstaculo obs, suciedad = s, corral = c, robots = r, ninos = Ninos ni} pos dir = if isValidPos nextLast n m && isEmpty ambiente nextLast then Ambiente {dimetions = (n, m), suciedad = s, corral = c, robots = r, ninos = Ninos (updateChildren ni movObs pos posfirstObst), obstaculos = Obstaculo movObs} else ambiente
  where
    posfirstObst = adyacentesPos pos !! dir
    allObst = getAllObstaculosInDirections ambiente posfirstObst dir
    len = length allObst
    lastObst = allObst !! (len -1)
    nextLast = adyacentesPos lastObst !! dir
    movObs = movListObst obs allObst dir

movOneChildren :: Ambiente -> (Int, Int) -> StdGen -> Ambiente
movOneChildren ambiente posChildren gen =
  let pos = head (randomNumber 3 gen)
   in movObstaculos ambiente posChildren pos

test :: Ambiente -> Bool
test ambiente = isEmpty ambiente (2, 3)

test2 :: Bool
test2 = test Ambiente {ninos = Ninos [(2, 3)], robots = Robot [(1, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4)], dimetions = (5, 5)}

test3 :: [(Int, Int)]
test3 = let amb = Ambiente {ninos = Ninos [(2, 3)], robots = Robot [(1, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4), (1, 5)], dimetions = (5, 6)} in getAllObstaculosInDirections amb (1, 3) 0

testMovObst :: Int -> Ambiente
testMovObst pos =
  let amb = Ambiente {ninos = Ninos [(2, 3)], robots = Robot [(2, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4)], dimetions = (5, 6)}
   in movObstaculos amb (1, 3) pos

testMOveOneChildren :: StdGen -> Ambiente
testMOveOneChildren = movOneChildren Ambiente {ninos = Ninos [(1, 2)], robots = Robot [(2, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4)], dimetions = (5, 6)} (1, 2)