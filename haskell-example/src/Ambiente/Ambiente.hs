module Ambiente.Ambiente
  ( 
    generateAmbiente,
    moveAllChildren,
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
import Utils (adyacentesPos, createSquare, isValidPos, randomNumber)

--ambiente 
data Ambiente = Ambiente
  { ninos :: Ninos,
    robots :: Robot,
    corral :: Corral,
    suciedad :: Suciedad,
    obstaculos :: Obstaculo,
    dimetions :: (Int, Int)
  }
  deriving (Show)

--revisa si el ambiente en esa posicion esta vacio
isEmpty :: Ambiente -> (Int, Int) -> Bool
isEmpty ambiente@Ambiente {ninos = n, robots = r, obstaculos = o, suciedad = s, corral = c} pos = not (isninosInPos n pos || isRobotInPos r pos || isobstaculoInPos o pos || issuciedadInPos s pos || iscorralInPos c pos)

--resive un ambiente y una posicion y retorna verdadero si hay un nino en el corral en esa posicion
isNinosInCorral :: Ambiente -> (Int,Int) -> Bool
isNinosInCorral ambiente@Ambiente{ninos = ni,corral = cor} pos = isninosInPos ni pos && iscorralInPos cor pos

--resive un ambiente y una posicion y retorna verdadero si hay un nino sobre un robot en esa posicion
isNinoUpRobot ::Ambiente -> (Int,Int) -> Bool 
isNinoUpRobot ambiente@Ambiente{ninos = ni,robots = rob} pos = isninosInPos ni pos && isRobotInPos rob pos

--resive el ambiente una posicion y una direccion y me retorna todos los obstaculos que se encuentran seguidos en esa direccion
getAllObstaculosInDirections :: Ambiente -> (Int, Int) -> Int -> [(Int, Int)]
getAllObstaculosInDirections ambiente@Ambiente {dimetions = (n, m), obstaculos = o} pos dir = if isValidPos postoMov n m && not (isEmpty ambiente postoMov) && isobstaculoInPos o postoMov then pos : getAllObstaculosInDirections ambiente postoMov dir else [pos]
  where
    postoMov = adyacentesPos pos !! dir

--mueve todos los obstaculos a partir de una posicion y una direccion 
movObstaculos :: Ambiente -> (Int, Int) -> Int -> Ambiente
movObstaculos ambiente@Ambiente {dimetions = (n, m), obstaculos = Obstaculo obs, suciedad = s, corral = c, robots = r, ninos = Ninos ni} pos dir = if isValidPos nextLast n m && isEmpty ambiente nextLast then Ambiente {dimetions = (n, m), suciedad = s, corral = c, robots = r, ninos = Ninos (updateChildren ambiente pos posfirstObst), obstaculos = Obstaculo movObs} else ambiente
  where
    posfirstObst = adyacentesPos pos !! dir
    allObst = getAllObstaculosInDirections ambiente posfirstObst dir
    len = length allObst
    lastObst = allObst !! (len -1)
    nextLast = adyacentesPos lastObst !! dir
    movObs = movListObst obs allObst dir

--resive un ambiente , la posicion del nino que quiere mover y la direccion en la que quiere moverse y retorna el ambinete moviendo el nino en caso de ser valida la posicion
movOneChildren :: Ambiente -> (Int, Int) -> Int -> Ambiente
movOneChildren ambiente@Ambiente {obstaculos = obs, robots = rob, suciedad = suc, corral = corr, dimetions = dim@(n,m)} posChildren dir =
  if isEmpty ambiente (adyacentesPos posChildren !! dir) && isValidPos (adyacentesPos posChildren !! dir) n m
    then Ambiente {ninos = Ninos (updateChildren ambiente posChildren (adyacentesPos posChildren !! dir)), suciedad = suc, robots = rob, obstaculos = obs, dimetions = dim, corral = corr}
    else movObstaculos ambiente posChildren dir


--resive el ambiente y un generador de numeros aleatorios para generar las direcciones de movimiento de los ninos y si estos deciden moverse o no 
-- y retorna el ambiente con los movimientos realizados
moveAllChildren :: Ambiente -> StdGen -> Ambiente
moveAllChildren ambiente@Ambiente {ninos = Ninos ni} gen =
  let posis = take (length ni) (randomNumber 3 gen)
      movOrNot = take (length ni) (randomNumber 1 gen)
   in moveAllChildren1 ambiente posis movOrNot ni

--funcion secundaria para mover los ninos, resive el ambiente las direcciones del movimiento y si estos se moveran o no,
-- y la lista de los ninos a mover y retorna el ambiente con los movimientos realizados
moveAllChildren1 :: Ambiente -> [Int] -> [Int] -> [(Int, Int)] -> Ambiente
moveAllChildren1 ambiente [] _ _ = ambiente
moveAllChildren1 ambiente _ [] _ = ambiente
moveAllChildren1 ambiente _ _ [] = ambiente
moveAllChildren1 ambiente (x : xs) (z : zs) (y : ys) =
  if z == 1
    then moveAllChildren1 (movOneChildren ambiente y x) xs zs ys
    else moveAllChildren1 ambiente xs zs ys

--cambia la posicion de un nino por la nueva posicion, en caso de que este pueda moverse
updateChildren :: Ambiente -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
updateChildren ambiente@Ambiente {ninos = Ninos ni, obstaculos = obs} pos1 pos2 =
  if not (isEmpty ambiente pos2) && not (isobstaculoInPos obs pos2)
    then ni
    else updateChildren2 ni pos1 pos2

--genera el ambiente donde se realiza la simulacion
generateAmbiente :: Int -> Int -> Int -> Int -> Int -> Int -> StdGen -> StdGen -> Ambiente
generateAmbiente n m cantNinos cantObst cantBasura cantRob  gen1 gen2 =
  let posX = randomNumber (n -1) gen1
      posY = randomNumber (m -1) gen2
      ambCorral = generateCorral n m (head posX, head posY) cantNinos
      ambNinos = generateNinos ambCorral posX posY cantNinos
      ambObst = generateObs ambNinos posX posY cantObst
      ambSuciedad = generateSuciedad ambObst posX posY cantBasura
   in generateRobots ambSuciedad posX posY cantRob

--genera el corral 
generateCorral :: Int -> Int -> (Int, Int) -> Int -> Ambiente
generateCorral n m initPos c =
  let corral = createSquare initPos n m c
   in Ambiente {ninos = Ninos [], robots = Robot [], corral = Corral corral, suciedad = Suciedad [], obstaculos = Obstaculo [], dimetions = (n, m)}

--genera los ninos
generateNinos :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateNinos ambiente (x : xs) (y : ys) 0 = ambiente
generateNinos ambiente _ [] _ = ambiente
generateNinos ambiente [] _ _ = ambiente
generateNinos ambiente@Ambiente {ninos = Ninos ni, obstaculos = obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateNinos Ambiente {ninos = Ninos ((x, y) : ni), obstaculos = obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateNinos ambiente xs ys c

--genera los obstaculos
generateObs :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateObs ambiente (x : xs) (y : ys) 0 = ambiente
generateObs ambiente _ [] _ = ambiente
generateObs ambiente [] _ _ = ambiente
generateObs ambiente@Ambiente {ninos = ni, obstaculos = Obstaculo obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateObs Ambiente {ninos = ni, obstaculos = Obstaculo ((x, y) : obs), robots = rob, suciedad = suc, corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateObs ambiente xs ys c

--gernera la suciedad
generateSuciedad :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateSuciedad ambiente (x : xs) (y : ys) 0 = ambiente
generateSuciedad ambiente _ [] _ = ambiente
generateSuciedad ambiente [] _ _ = ambiente
generateSuciedad ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = rob, suciedad = Suciedad suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateSuciedad Ambiente {ninos = ni, obstaculos = obs, robots = rob, suciedad = Suciedad ((x, y) : suc), corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateSuciedad ambiente xs ys c

--genera los robots
generateRobots :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateRobots ambiente (x : xs) (y : ys) 0 = ambiente
generateRobots ambiente _ [] _ = ambiente
generateRobots ambiente [] _ _ = ambiente
generateRobots ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = Robot rob, suciedad = suc, corral = cor, dimetions = (n, m)} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateRobots Ambiente {ninos = ni, obstaculos = obs, robots = Robot ((x, y) : rob), suciedad = suc, corral = cor, dimetions = (n, m)} xs ys (c -1)
    else generateRobots ambiente xs ys c

