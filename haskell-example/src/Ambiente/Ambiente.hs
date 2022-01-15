module Ambiente.Ambiente
  ( test,
    test2,
    test3,
    testMovObst,
    testMOveAllChildren,
    generateAmbiente,
    Ambiente (..),
    Ninos (..),
    Suciedad (..),
    Obstaculo (..),
    Robot (..),
    Corral (..),
  )
where

import Elementos.Corral (Corral (Corral), iscorralInPos)
import Elementos.Ninos (Ninos (Ninos), isninosInPos, updateChildren2)
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
movObstaculos ambiente@Ambiente {dimetions = (n, m), obstaculos = Obstaculo obs, suciedad = s, corral = c, robots = r, ninos = Ninos ni} pos dir = if isValidPos nextLast n m && isEmpty ambiente nextLast then Ambiente {dimetions = (n, m), suciedad = s, corral = c, robots = r, ninos = Ninos (updateChildren ambiente pos posfirstObst), obstaculos = Obstaculo movObs} else ambiente
  where
    posfirstObst = adyacentesPos pos !! dir
    allObst = getAllObstaculosInDirections ambiente posfirstObst dir
    len = length allObst
    lastObst = allObst !! (len -1)
    nextLast = adyacentesPos lastObst !! dir
    movObs = movListObst obs allObst dir

movOneChildren :: Ambiente -> (Int, Int) -> Int -> Ambiente
movOneChildren ambiente@Ambiente {obstaculos = obs, robots = rob, suciedad = suc, corral = corr, dimetions = dim} posChildren dir = if isEmpty ambiente (adyacentesPos posChildren !! dir) then Ambiente {ninos = Ninos (updateChildren ambiente posChildren (adyacentesPos posChildren !! dir)), suciedad = suc, robots = rob, obstaculos = obs, dimetions = dim, corral = corr} else movObstaculos ambiente posChildren dir

moveAllChildren :: Ambiente -> StdGen -> Ambiente
moveAllChildren ambiente@Ambiente {ninos = Ninos ni} gen =
  let posis = take (length ni) (randomNumber 3 gen)
      movOrNot = take (length ni) (randomNumber 1 gen)
   in moveAllChildren1 ambiente posis movOrNot ni

moveAllChildren1 :: Ambiente -> [Int] -> [Int] -> [(Int, Int)] -> Ambiente
moveAllChildren1 ambiente [] _ _ = ambiente
moveAllChildren1 ambiente _ [] _ = ambiente
moveAllChildren1 ambiente _ _ [] = ambiente
moveAllChildren1 ambiente (x : xs) (z : zs) (y : ys) = if z == 1 then moveAllChildren1 (movOneChildren ambiente y x) xs zs ys else moveAllChildren1 ambiente xs zs ys

updateChildren :: Ambiente -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
updateChildren ambiente@Ambiente {ninos = Ninos ni, obstaculos = obs} pos1 pos2 = if not (isEmpty ambiente pos2) && not (isobstaculoInPos obs pos2) then ni else updateChildren2 ni pos1 pos2

generateAmbiente :: Int -> Int -> Int -> Int -> Int -> Int -> StdGen -> StdGen -> Ambiente
generateAmbiente n m cantNinos cantObst cantBasura cantRob gen1 gen2 =
  let posX = randomNumber (n -1) gen1
      posY = randomNumber (m -1) gen2
      ambNinos = generateNinos Ambiente {ninos = Ninos [], robots = Robot [], corral = Corral [], suciedad = Suciedad [], obstaculos = Obstaculo [], dimetions = (n, m)} posX posY cantNinos
      ambObst = generateObs ambNinos posX posY cantObst
      ambSuciedad = generateSuciedad ambObst posX posY cantBasura
  in generateRobots ambSuciedad posX posY cantRob    

generateNinos :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateNinos ambiente (x : xs) (y : ys) 0 = ambiente
generateNinos ambiente _ [] _ = ambiente
generateNinos ambiente [] _ _ = ambiente
generateNinos ambiente@Ambiente {ninos = Ninos ni, obstaculos = obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateNinos Ambiente {ninos = Ninos ((x, y) : ni), obstaculos = obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateNinos ambiente xs ys c

generateObs :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateObs ambiente (x : xs) (y : ys) 0 = ambiente
generateObs ambiente _ [] _ = ambiente
generateObs ambiente [] _ _ = ambiente
generateObs ambiente@Ambiente {ninos = ni, obstaculos = Obstaculo obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateObs Ambiente {ninos = ni, obstaculos = Obstaculo ((x, y) : obs), robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateObs ambiente xs ys c

generateSuciedad :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateSuciedad ambiente (x : xs) (y : ys) 0 = ambiente
generateSuciedad ambiente _ [] _ = ambiente
generateSuciedad ambiente [] _ _ = ambiente
generateSuciedad ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = rob, suciedad = Suciedad suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateSuciedad Ambiente {ninos = ni, obstaculos = obs, robots = rob, suciedad = Suciedad ((x, y) : suc), corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateSuciedad ambiente xs ys c

generateRobots :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateRobots ambiente (x : xs) (y : ys) 0 = ambiente
generateRobots ambiente _ [] _ = ambiente
generateRobots ambiente [] _ _ = ambiente
generateRobots ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = Robot rob, suciedad = suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateRobots Ambiente {ninos = ni, obstaculos = obs, robots = Robot ((x, y) : rob), suciedad = suc, corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateRobots ambiente xs ys c

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

testMOveAllChildren :: StdGen -> Ambiente
testMOveAllChildren = moveAllChildren Ambiente {ninos = Ninos [(1, 2), (2, 2)], robots = Robot [(2, 4)], corral = Corral [(4, 2)], suciedad = Suciedad [(2, 3)], obstaculos = Obstaculo [(1, 3), (1, 4)], dimetions = (5, 6)}