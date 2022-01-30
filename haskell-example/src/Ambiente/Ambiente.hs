module Ambiente.Ambiente
  ( generateAmbiente,
    moveAllChildren,
    bFSNinos,
    getPath,
    selectMovtoNino,
    robotAgentCargaNino,
    robotAgentClean,
    testSelectMovToCorral,
    testSekctMovToNino,
    changeAmbiente,
    getSuciedad,
    calculatePorcentSuciedad,
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
import Utils (adyacentesPos, adyacentesPosToSquare, createSquare, distance, getfirstElement, getsecondElement, isContain, isValidPos, randomNumber, remove, update)

--ambiente
data Ambiente = Ambiente
  { ninos :: Ninos,
    robots :: Robot,
    corral :: Corral,
    suciedad :: Suciedad,
    obstaculos :: Obstaculo,
    dimetions :: (Int, Int),
    robotChargeNino :: [((Int, Int), Bool)],
    posMidelCorral :: (Int, Int)
  }
  deriving (Show)

--revisa si el ambiente en esa posicion esta vacio
isEmpty :: Ambiente -> (Int, Int) -> Bool
isEmpty ambiente@Ambiente {ninos = n, robots = r, obstaculos = o, suciedad = s, corral = c} pos = not (isninosInPos n pos || isRobotInPos r pos || isobstaculoInPos o pos || issuciedadInPos s pos || iscorralInPos c pos)

--resive un ambiente y una posicion y retorna verdadero si hay un nino en el corral en esa posicion
isNinosInCorral :: Ambiente -> (Int, Int) -> Bool
isNinosInCorral ambiente@Ambiente {ninos = ni, corral = cor} pos = isninosInPos ni pos && iscorralInPos cor pos

--resive un ambiente y una posicion y retorna verdadero si hay un nino sobre un robot en esa posicion
isNinoUpRobot :: Ambiente -> (Int, Int) -> Bool
isNinoUpRobot ambiente@Ambiente {robotChargeNino = robN} = getsecondElement robN False

isRobotInCorral :: Ambiente -> (Int, Int) -> Bool
isRobotInCorral ambiente@Ambiente {robots = rob, corral = cor} pos = isRobotInPos rob pos && iscorralInPos cor pos

--chequea si existe algun nino que no este en el corral
existFreeNino :: Ambiente -> Bool
existFreeNino ambiente@Ambiente {ninos = Ninos ni} = let ninosFree = [n | n <- ni, not (isNinosInCorral ambiente n)] in length ninosFree > 0

--resive el ambiente una posicion y una direccion y me retorna todos los obstaculos que se encuentran seguidos en esa direccion
getAllObstaculosInDirections :: Ambiente -> (Int, Int) -> Int -> [(Int, Int)]
getAllObstaculosInDirections ambiente@Ambiente {dimetions = (n, m), obstaculos = o} pos dir = if isValidPos postoMov n m && not (isEmpty ambiente postoMov) && isobstaculoInPos o postoMov then pos : getAllObstaculosInDirections ambiente postoMov dir else [pos]
  where
    postoMov = adyacentesPosToSquare pos !! dir

--mueve todos los obstaculos a partir de una posicion y una direccion
movObstaculos :: Ambiente -> (Int, Int) -> Int -> StdGen -> Ambiente
movObstaculos ambiente@Ambiente {dimetions = (n, m), obstaculos = Obstaculo obs, suciedad = Suciedad suc, corral = c, robots = r, ninos = Ninos ni, robotChargeNino = robN, posMidelCorral = posM} pos dir gen =
  if isValidPos nextLast n m && isEmpty ambiente nextLast
    then --poner aqui que ensucie los ninos

      let newChildren@(ch, is) = updateChildren ambiente pos posfirstObst
          tempAmb@Ambiente {ninos = ni2} = Ambiente {dimetions = (n, m), suciedad = Suciedad suc, corral = c, robots = r, ninos = Ninos ch, obstaculos = Obstaculo movObs, robotChargeNino = robN, posMidelCorral = posM}
       in if is
            then Ambiente {suciedad = Suciedad (suc ++ generateSuciedadWithNino tempAmb pos posfirstObst gen), dimetions = (n, m), corral = c, robots = r, ninos = ni2, obstaculos = Obstaculo movObs, robotChargeNino = robN, posMidelCorral = posM}
            else Ambiente {suciedad = Suciedad suc, dimetions = (n, m), corral = c, robots = r, ninos = ni2, obstaculos = Obstaculo movObs, robotChargeNino = robN, posMidelCorral = posM}
    else ambiente
  where
    posfirstObst = adyacentesPosToSquare pos !! dir
    allObst = getAllObstaculosInDirections ambiente posfirstObst dir
    len = length allObst
    lastObst = allObst !! (len -1)
    nextLast = adyacentesPosToSquare lastObst !! dir
    movObs = movListObst obs allObst dir

--resive un ambiente , la posicion del nino que quiere mover y la direccion en la que quiere moverse y retorna el ambinete moviendo el nino en caso de ser valida la posicion
movOneChildren :: Ambiente -> (Int, Int) -> Int -> StdGen -> Ambiente
movOneChildren ambiente@Ambiente {obstaculos = obs, robots = rob, suciedad = Suciedad suc, corral = corr, dimetions = dim@(n, m), robotChargeNino = robN, posMidelCorral = posM} posChildren dir gen
  | not (isValidPos (adyacentesPosToSquare posChildren !! dir) n m) = ambiente
  --poner aqui que ensucie los ninos
  | isValidPos (adyacentesPosToSquare posChildren !! dir) n m && isEmpty ambiente (adyacentesPosToSquare posChildren !! dir) =
    let newChildren@(ch, is) = updateChildren ambiente posChildren (adyacentesPosToSquare posChildren !! dir)
        tempAmb = Ambiente {ninos = Ninos ch, suciedad = Suciedad suc, robots = rob, obstaculos = obs, dimetions = dim, corral = corr, robotChargeNino = robN, posMidelCorral = posM}
     in if is
          then Ambiente {suciedad = Suciedad (suc ++ generateSuciedadWithNino tempAmb posChildren (adyacentesPosToSquare posChildren !! dir) gen), ninos = Ninos ch, robots = rob, obstaculos = obs, dimetions = dim, corral = corr, robotChargeNino = robN, posMidelCorral = posM}
          else Ambiente {suciedad = Suciedad suc, ninos = Ninos ch, robots = rob, obstaculos = obs, dimetions = dim, corral = corr, robotChargeNino = robN, posMidelCorral = posM}
  | isobstaculoInPos obs (adyacentesPosToSquare posChildren !! dir) = movObstaculos ambiente posChildren dir gen
  | otherwise = ambiente

--resive el ambiente y un generador de numeros aleatorios para generar las direcciones de movimiento de los ninos y si estos deciden moverse o no
-- y retorna el ambiente con los movimientos realizados
moveAllChildren :: Ambiente -> StdGen -> Ambiente
moveAllChildren ambiente@Ambiente {ninos = Ninos ni} gen =
  let posis = take (length ni) (randomNumber 7 gen)
      movOrNot = take (length ni) (randomNumber 1 gen)
   in moveAllChildren1 ambiente posis movOrNot ni gen

--funcion secundaria para mover los ninos, resive el ambiente las direcciones del movimiento y si estos se moveran o no,
-- y la lista de los ninos a mover y retorna el ambiente con los movimientos realizados
moveAllChildren1 :: Ambiente -> [Int] -> [Int] -> [(Int, Int)] -> StdGen -> Ambiente
moveAllChildren1 ambiente [] _ _ _ = ambiente
moveAllChildren1 ambiente _ [] _ _ = ambiente
moveAllChildren1 ambiente _ _ [] _ = ambiente
moveAllChildren1 ambiente (x : xs) (z : zs) (y : ys) gen =
  if z == 1
    then moveAllChildren1 (movOneChildren ambiente y x gen) xs zs ys gen
    else moveAllChildren1 ambiente xs zs ys gen

--cambia la posicion de un nino por la nueva posicion, en caso de que este pueda moverse
updateChildren :: Ambiente -> (Int, Int) -> (Int, Int) -> ([(Int, Int)], Bool)
updateChildren ambiente@Ambiente {ninos = Ninos ni, obstaculos = obs} pos1 pos2 =
  if not (isNinosInCorral ambiente pos1) && (isEmpty ambiente pos2 || isobstaculoInPos obs pos2)
    then (updateChildren2 ni pos1 pos2, True)
    else (ni, False)

changeAmbiente :: Ambiente -> StdGen -> StdGen -> Ambiente
changeAmbiente ambiente@Ambiente {dimetions = (n, m), corral = Corral cor, ninos = Ninos ni, robots = Robot rob, suciedad = Suciedad suc, obstaculos = Obstaculo obs, robotChargeNino = rcn} gen1 gen2 =
  let posX = randomNumber (n -1) gen1
      posY = randomNumber (m -1) gen2
      ambCorral = generateCorral n m (head posX, head posY) (length cor)
      ambNinos = generateNinos ambCorral posX posY (length ni)
      ambRobot = generateRobots ambNinos posX posY (length rob)
      ambSuciedad = generateSuciedad ambRobot posX posY (length suc)
      ambObst = generateObs ambSuciedad posX posY (length obs)
   in addAmbRobotsChargeNinos ambObst rcn

addAmbRobotsChargeNinos :: Ambiente -> [((Int, Int), Bool)] -> Ambiente
addAmbRobotsChargeNinos ambiente@Ambiente {dimetions = dim, corral = cor, ninos = ni, robots = Robot rob, suciedad = suc, obstaculos = obs, posMidelCorral = posM} robtsCharNinos = Ambiente {robotChargeNino = addToAmbNinosUpRobot robtsCharNinos rob, dimetions = dim, corral = cor, ninos = ni, robots = Robot rob, suciedad = suc, obstaculos = obs, posMidelCorral = posM}

addToAmbNinosUpRobot :: [((Int, Int), Bool)] -> [(Int, Int)] -> [((Int, Int), Bool)]
addToAmbNinosUpRobot [] _ = []
addToAmbNinosUpRobot _ [] = []
addToAmbNinosUpRobot robotsCharchNinosOld@((p, is) : xs) robotsNew@(r : rs) = (r, is) : addToAmbNinosUpRobot xs rs

--genera el ambiente donde se realiza la simulacion
generateAmbiente :: Int -> Int -> Int -> Int -> Int -> Int -> StdGen -> StdGen -> Ambiente
generateAmbiente n m cantNinos cantObst cantBasura cantRob gen1 gen2 =
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
   in Ambiente {ninos = Ninos [], robots = Robot [], corral = Corral corral, suciedad = Suciedad [], obstaculos = Obstaculo [], dimetions = (n, m), robotChargeNino = [], posMidelCorral = initPos}

--genera los ninos
generateNinos :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateNinos ambiente (x : xs) (y : ys) 0 = ambiente
generateNinos ambiente _ [] _ = ambiente
generateNinos ambiente [] _ _ = ambiente
generateNinos ambiente@Ambiente {ninos = Ninos ni, obstaculos = obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m), robotChargeNino = robN, posMidelCorral = posM} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateNinos Ambiente {ninos = Ninos ((x, y) : ni), obstaculos = obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m), robotChargeNino = robN, posMidelCorral = posM} xs ys (c -1)
    else generateNinos ambiente xs ys c

--genera los obstaculos
generateObs :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateObs ambiente (x : xs) (y : ys) 0 = ambiente
generateObs ambiente _ [] _ = ambiente
generateObs ambiente [] _ _ = ambiente
generateObs ambiente@Ambiente {ninos = ni, obstaculos = Obstaculo obs, robots = rob, suciedad = suc, corral = cor, dimetions = (n, m), robotChargeNino = robN, posMidelCorral = posM} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateObs Ambiente {ninos = ni, obstaculos = Obstaculo ((x, y) : obs), robots = rob, suciedad = suc, corral = cor, dimetions = (n, m), robotChargeNino = robN, posMidelCorral = posM} xs ys (c -1)
    else generateObs ambiente xs ys c

--gernera la suciedad
generateSuciedad :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateSuciedad ambiente (x : xs) (y : ys) 0 = ambiente
generateSuciedad ambiente _ [] _ = ambiente
generateSuciedad ambiente [] _ _ = ambiente
generateSuciedad ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = rob, suciedad = Suciedad suc, corral = cor, dimetions = (n, m), robotChargeNino = robN, posMidelCorral = posM} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateSuciedad Ambiente {ninos = ni, obstaculos = obs, robots = rob, suciedad = Suciedad ((x, y) : suc), corral = cor, dimetions = (n, m), robotChargeNino = robN, posMidelCorral = posM} xs ys (c -1)
    else generateSuciedad ambiente xs ys c

--genera los robots
generateRobots :: Ambiente -> [Int] -> [Int] -> Int -> Ambiente
generateRobots ambiente (x : xs) (y : ys) 0 = ambiente
generateRobots ambiente _ [] _ = ambiente
generateRobots ambiente [] _ _ = ambiente
generateRobots ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = Robot rob, suciedad = suc, corral = cor, dimetions = (n, m), robotChargeNino = robN, posMidelCorral = posM} (x : xs) (y : ys) c =
  if isEmpty ambiente (x, y)
    then generateRobots Ambiente {ninos = ni, obstaculos = obs, robots = Robot ((x, y) : rob), suciedad = suc, corral = cor, dimetions = (n, m), robotChargeNino = ((x, y), False) : robN, posMidelCorral = posM} xs ys (c -1)
    else generateRobots ambiente xs ys c

--agente que carga primero a los ninos
robotAgentCargaNino :: Ambiente -> Ambiente
robotAgentCargaNino ambiente@Ambiente {robots = Robot r} = moveAllRobotAgentCragaNinos ambiente r

--agente que limpia primero
robotAgentClean :: Ambiente -> Ambiente
robotAgentClean ambiente@Ambiente {robots = Robot r} = moveAllRobotAgentClean ambiente r

moveAllRobotAgentCragaNinos :: Ambiente -> [(Int, Int)] -> Ambiente
moveAllRobotAgentCragaNinos ambiente [] = ambiente
moveAllRobotAgentCragaNinos ambiente robots@(r : rs) = moveAllRobotAgentCragaNinos (moveOneRobotAgent ambiente r) rs

moveAllRobotAgentClean :: Ambiente -> [(Int, Int)] -> Ambiente
moveAllRobotAgentClean ambiente [] = ambiente
moveAllRobotAgentClean ambiente robots@(r : rs) = moveAllRobotAgentClean (moveOneRobotAgent1 ambiente r) rs

--mover el robot que se encarga de recoger primero los ninos
moveOneRobotAgent :: Ambiente -> (Int, Int) -> Ambiente
moveOneRobotAgent ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = Robot rob, suciedad = Suciedad suc, corral = cor, dimetions = dim, robotChargeNino = robN, posMidelCorral = posM} pos
  | issuciedadInPos (Suciedad suc) pos = clean ambiente pos --poner que la recoja
  | isNinoUpRobot ambiente pos = selectMovtoCorral ambiente pos
  | is && not (isNinosInCorral ambiente posNino) && not (isNinosInCorral ambiente pos) = cargaNino ambiente posNino pos --si hay una nino alcanzable cojelo
  | not (isNinoUpRobot ambiente pos) && existFreeNino ambiente --si no hay un nino alzanzable ni ensima del robot busca el nino mas sercano(falta poner si existe algun nino a buscar)
    =
    let toMov = selectMovtoNino ambiente pos --seleccionar la mejor posicion a moverse
     in if toMov /= (-1, -1)
          then
            let robotNewPos = update rob pos toMov --mover el robot
                es = getsecondElement robN False pos --buscar el estado del robot(si hay alguien ensima de el)
                newRobotChargeNino = update robN (pos, es) (toMov, es) --cambiar la posicion del robot en la lista de si tiene a alguien arriba
             in Ambiente {ninos = ni, robots = Robot robotNewPos, obstaculos = obs, suciedad = Suciedad suc, dimetions = dim, robotChargeNino = newRobotChargeNino, corral = cor, posMidelCorral = posM}
          else ambiente
  | length suc > 0 = selectMovToSuciedad ambiente pos
  | otherwise = ambiente --poner para que busque la basura mas cercana
  where
    isNinoAdy@(is, posNino) = checkAdyNinos ambiente pos

--mover el robot que se encarga de recoger primero la basura
moveOneRobotAgent1 :: Ambiente -> (Int, Int) -> Ambiente
moveOneRobotAgent1 ambiente@Ambiente {ninos = ni, obstaculos = obs, robots = Robot rob, suciedad = Suciedad suc, corral = cor, dimetions = dim, robotChargeNino = robN, posMidelCorral = posM} pos
  | issuciedadInPos (Suciedad suc) pos = clean ambiente pos --poner que la recoja
  | isNinoUpRobot ambiente pos = selectMovtoCorral ambiente pos
  | is && not (isNinosInCorral ambiente posNino) && not (isNinosInCorral ambiente pos) = cargaNino ambiente posNino pos --si hay una nino alcanzable cojelo
  | length suc > 0 = selectMovToSuciedad ambiente pos
  | not (isNinoUpRobot ambiente pos) && existFreeNino ambiente --si no hay un nino alzanzable ni ensima del robot busca el nino mas sercano(falta poner si existe algun nino a buscar)
    =
    let toMov = selectMovtoNino ambiente pos --seleccionar la mejor posicion a moverse
     in if toMov /= (-1, -1)
          then
            let robotNewPos = update rob pos toMov --mover el robot
                es = getsecondElement robN False pos --buscar el estado del robot(si hay alguien ensima de el)
                newRobotChargeNino = update robN (pos, es) (toMov, es) --cambiar la posicion del robot en la lista de si tiene a alguien arriba
             in Ambiente {ninos = ni, robots = Robot robotNewPos, obstaculos = obs, suciedad = Suciedad suc, dimetions = dim, robotChargeNino = newRobotChargeNino, corral = cor, posMidelCorral = posM}
          else ambiente
  | otherwise = ambiente --poner para que busque la basura mas cercana
  where
    isNinoAdy@(is, posNino) = checkAdyNinos ambiente pos

--terminar esta funcion para generar la suciedad en el ambiente creada por los ninos
generateSuciedadWithNino :: Ambiente -> (Int, Int) -> (Int, Int) -> StdGen -> [(Int, Int)]
generateSuciedadWithNino ambiente@Ambiente {dimetions = (n, m), corral = cor} pos posToMov gen =
  let scuare = [a | a <- adyacentesPosToSquare pos, isValidPos a n m, not (iscorralInPos cor a)] --cuadra
      countNinos = countNinosInSquare ambiente scuare -1 --cantidad de ninos en la cuadricula de 3x3
      emptyPos = getEmptyPosInSquare ambiente scuare --casillas vacias en la cuadricuala de 3x3
      cantToEnsuciar = getCantSuciedadGenerate countNinos gen --cantidad de casillas que van ha ser ensuciadas
   in ensusiarPosEmpty emptyPos cantToEnsuciar gen

--cuenta la cantidad de ninos que hay en la cuadricula de 3x3 de centro en la posicion del nino
countNinosInSquare :: Ambiente -> [(Int, Int)] -> Int
countNinosInSquare _ [] = 0
countNinosInSquare ambiente@Ambiente {ninos = ni, corral = cor} square@(s : xs) = if isninosInPos ni s && not (iscorralInPos cor s) then 1 + countNinosInSquare ambiente xs else countNinosInSquare ambiente xs

getEmptyPosInSquare :: Ambiente -> [(Int, Int)] -> [(Int, Int)]
getEmptyPosInSquare _ [] = []
getEmptyPosInSquare ambiente square@(p : ps) = if isEmpty ambiente p then p : getEmptyPosInSquare ambiente ps else getEmptyPosInSquare ambiente ps

getCantSuciedadGenerate :: Int -> StdGen -> Int
getCantSuciedadGenerate cantNinos gen
  | cantNinos == 0 = head (randomNumber 1 gen)
  | cantNinos == 1 = head (randomNumber 2 gen)
  | otherwise = head (randomNumber 6 gen)

ensusiarPosEmpty :: [(Int, Int)] -> Int -> StdGen -> [(Int, Int)]
ensusiarPosEmpty [] _ _ = []
ensusiarPosEmpty _ 0 _ = []
ensusiarPosEmpty emptyPos cantToEnsuciar gen =
  let pos = head (randomNumber (length emptyPos -1) gen)
      dirtyPos = emptyPos !! pos
   in dirtyPos : ensusiarPosEmpty (remove emptyPos dirtyPos) (cantToEnsuciar -1) gen

myMap :: [(Int, Int)] -> Int -> [((Int, Int), Int)]
myMap [] _ = []
myMap (x : xs) d = (x, d) : myMap xs d

isContainInFather :: [((Int, Int), (Int, Int))] -> (Int, Int) -> Bool
isContainInFather [] _ = False
isContainInFather ((a, b) : xs) child = (a == child) || isContainInFather xs child

getFather :: [((Int, Int), (Int, Int))] -> (Int, Int) -> (Int, Int)
getFather [] child = child
getFather ((c, f) : xs) child = if c == child then f else getFather xs child

--recorrido para encontrar al nino mas sercano (ver si poner que los robots tambien son obstaculos)
bFSNinos :: Ambiente -> (Int, Int) -> [((Int, Int), Int)] -> [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), (Int, Int))] -> ([((Int, Int), Int)], [((Int, Int), (Int, Int))])
bFSNinos _ _ [] _ dist father = (dist, father)
bFSNinos _ _ _ _ [] _ = ([], [])
bFSNinos ambiente@Ambiente {obstaculos = obs, dimetions = (n, m), ninos = ni, corral = cor, robots = rob} initpos pila@((p, d) : ps) visit dist father
  | isContain visit p || isobstaculoInPos obs p || (isRobotInPos rob p && p /= initpos) = bFSNinos ambiente initpos ps visit dist father
  | isninosInPos ni p && not (isContain (adyacentesPosToSquare initpos) p) && not (isNinosInCorral ambiente p) && not (isRobotInCorral ambiente p) = ((p, d) : dist, father)
  | isninosInPos ni p && isContain (adyacentesPosToSquare initpos) p = bFSNinos ambiente initpos ps visit dist father
  | otherwise = bFSNinos ambiente initpos (ps ++ myMap ([h | h <- adyacentesPosToSquare p, isValidPos h n m]) (d + 1)) (p : visit) ((p, d) : dist) (father ++ [(f, p) | f <- adyacentesPosToSquare p, isValidPos f n m, not (isContainInFather father f)])

--recorrido para encontrar al corral
bFSCorral :: Ambiente -> (Int, Int) -> [((Int, Int), Int)] -> [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), (Int, Int))] -> ([((Int, Int), Int)], [((Int, Int), (Int, Int))])
bFSCorral _ _ [] _ dist father = (dist, father)
bFSCorral _ _ _ _ [] _ = ([], [])
bFSCorral ambiente@Ambiente {suciedad = suc, dimetions = (n, m), corral = cor, robots = rob, ninos = ni, obstaculos = obs} initPos pila@((p, d) : ps) visit dist father
  | isContain visit p || (isRobotInPos rob p && p /= initPos) || isninosInPos ni p || isobstaculoInPos obs p = bFSCorral ambiente initPos ps visit dist father
  | otherwise = bFSCorral ambiente initPos (ps ++ myMap ([h | h <- adyacentesPosToSquare p, isValidPos h n m]) (d + 1)) (p : visit) ((p, d) : dist) (father ++ [(f, p) | f <- adyacentesPosToSquare p, isValidPos f n m, not (isContainInFather father f)])

--recorrido para encontrar al corral
bFSSuciedad :: Ambiente -> (Int, Int) -> [((Int, Int), Int)] -> [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), (Int, Int))] -> ([((Int, Int), Int)], [((Int, Int), (Int, Int))])
bFSSuciedad _ _ [] _ dist father = (dist, father)
bFSSuciedad _ _ _ _ [] _ = ([], [])
bFSSuciedad ambiente@Ambiente {suciedad = suc, dimetions = (n, m), corral = cor, robots = rob, ninos = ni, obstaculos = obs} initPos pila@((p, d) : ps) visit dist father
  | isContain visit p || (isRobotInPos rob p && p /= initPos) || isninosInPos ni p && p /= initPos || isobstaculoInPos obs p = bFSSuciedad ambiente initPos ps visit dist father
  | issuciedadInPos suc p = ((p, d) : dist, father)
  | otherwise = bFSSuciedad ambiente initPos (ps ++ myMap ([h | h <- adyacentesPosToSquare p, isValidPos h n m]) (d + 1)) (p : visit) ((p, d) : dist) (father ++ [(f, p) | f <- adyacentesPosToSquare p, isValidPos f n m, not (isContainInFather father f)])

--retorna el caminno para llegar de una posicion a otra
getPathVisit :: (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))] -> [(Int, Int)]
getPathVisit posInit posEnd fathers = if posInit == posEnd then [posInit] else let p = getFather fathers posEnd in posEnd : getPathVisit posInit p fathers

getPath :: (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))] -> [(Int, Int)]
getPath posInit posEnd fathers = reverse (getPathVisit posInit posEnd fathers)

--selecciona la posicion que mas me acerca aun nino
selectMovtoNino :: Ambiente -> (Int, Int) -> (Int, Int)
selectMovtoNino ambiente@Ambiente {robots = Robot rob} posRobot =
  let dist@(d, f) = bFSNinos ambiente posRobot [(posRobot, 0)] [] [(posRobot, 0)] []
      pat = getPath posRobot (fst (head d)) f
   in if length pat > 1 then pat !! 1 else (-1, -1)

--selecciona la accion a realizar para llegar al corral o soltar el nino
selectMovtoCorral :: Ambiente -> (Int, Int) -> Ambiente
selectMovtoCorral ambiente@Ambiente {corral = Corral cor, dimetions = (n, m), posMidelCorral = posM} posRobot =
  let dist@(d, f) = bFSCorral ambiente posRobot [(posRobot, 0)] [] [(posRobot, 0)] []
      (posToMov, distToMidel) = selectMovtoCorralVisit ambiente d cor (n * m + 1) (-1, -1)
   in if posToMov == (-1, -1)
        then ambiente
        else
          if iscorralInPos (Corral cor) posRobot && distance posRobot posM == distToMidel
            then sueltaNino ambiente posRobot
            else moveRobotToCorral ambiente posRobot posToMov f

selectMovToSuciedad :: Ambiente -> (Int, Int) -> Ambiente
selectMovToSuciedad ambiente@Ambiente {ninos = ni, robotChargeNino = rcn, obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robots = Robot rob, posMidelCorral = posM} posRobot =
  let dist@(d, f) = bFSSuciedad ambiente posRobot [(posRobot, 0)] [] [(posRobot, 0)] []
      path = getPath posRobot (fst (head d)) f
   in if length path > 1
        then Ambiente {robots = Robot (update rob posRobot (path !! 1)), corral = cor, suciedad = suc, obstaculos = obs, dimetions = dim, robotChargeNino = update rcn (posRobot, False) (path !! 1, False), posMidelCorral = posM, ninos = ni}
        else ambiente

sueltaNino :: Ambiente -> (Int, Int) -> Ambiente
sueltaNino ambiente@Ambiente {ninos = Ninos ni, robotChargeNino = rcn, obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robots = rob, posMidelCorral = posM} pos = Ambiente {ninos = Ninos (pos : ni), obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robots = rob, posMidelCorral = posM, robotChargeNino = newRobotChargeNino}
  where
    newRobotChargeNino = update rcn (pos, True) (pos, False)

moveRobotToCorral :: Ambiente -> (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))] -> Ambiente
moveRobotToCorral ambiente@Ambiente {ninos = ni, robotChargeNino = rcn, obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robots = Robot rob, posMidelCorral = posM} posInit posEnd fathers =
  let path = getPath posInit posEnd fathers
   in if length path > 2
        then
          let toMov = path !! 2
           in Ambiente {robots = Robot (update rob posInit toMov), ninos = ni, obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robotChargeNino = update rcn (posInit, True) (toMov, True), posMidelCorral = posM}
        else
          let toMov = path !! 1
           in Ambiente {robots = Robot (update rob posInit toMov), ninos = ni, obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robotChargeNino = update rcn (posInit, True) (toMov, True), posMidelCorral = posM}

--selecciona la posicion en el corral mas cercana al centro que puede albergar un nino
selectMovtoCorralVisit :: Ambiente -> [((Int, Int), Int)] -> [(Int, Int)] -> Int -> (Int, Int) -> ((Int, Int), Int)
selectMovtoCorralVisit _ _ [] bestDist bestPos = (bestPos, bestDist)
selectMovtoCorralVisit ambiente@Ambiente {posMidelCorral = posM} dists corral@(x : xs) bestDist bestPos
  | isNinosInCorral ambiente x = selectMovtoCorralVisit ambiente dists xs bestDist bestPos
  | (distToPosCorral /= -1) && distToMidel < bestDist = selectMovtoCorralVisit ambiente dists xs distToMidel x --revisar esto aqui
  | otherwise = selectMovtoCorralVisit ambiente dists xs bestDist bestPos
  where
    (distToMidel, distToPosCorral) = (distance posM x, getsecondElement dists (-1) x)

checkAdyNinos :: Ambiente -> (Int, Int) -> (Bool, (Int, Int))
checkAdyNinos ambiente@Ambiente {dimetions = (n, m), ninos = ni} pos =
  let ady = [p | p <- adyacentesPosToSquare pos, isValidPos p n m, isninosInPos ni p, not (isNinosInCorral ambiente p)]
   in if null ady then (False, (-1, -1)) else (True, head ady)

cargaNino :: Ambiente -> (Int, Int) -> (Int, Int) -> Ambiente
cargaNino ambiente@Ambiente {ninos = Ninos ni, robotChargeNino = rcn, obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robots = Robot rob, posMidelCorral = posM} posNino posRobot =
  let newNinos = remove ni posNino
      newRobotChargeNino = update rcn (posRobot, False) (posNino, True)
      newRobot = update rob posRobot posNino
   in Ambiente {ninos = Ninos newNinos, robotChargeNino = newRobotChargeNino, obstaculos = obs, suciedad = suc, corral = cor, dimetions = dim, robots = Robot newRobot, posMidelCorral = posM}

clean :: Ambiente -> (Int, Int) -> Ambiente
clean ambiente@Ambiente {ninos = ni, robotChargeNino = rcn, obstaculos = obs, suciedad = Suciedad suc, corral = cor, dimetions = dim, robots = rob, posMidelCorral = posM} pos =
  let newSuciedad = remove suc pos
   in Ambiente {ninos = ni, robotChargeNino = rcn, obstaculos = obs, suciedad = Suciedad newSuciedad, corral = cor, dimetions = dim, robots = rob, posMidelCorral = posM}

testSelectMovToCorral :: Ambiente -> ([((Int, Int), Int)], [((Int, Int), (Int, Int))])
testSelectMovToCorral ambiente@Ambiente {robots = Robot rob, corral = Corral cor, dimetions = (n, m)} = let dist@(d, f) = bFSCorral ambiente (head rob) [(head rob, 0)] [] [(head rob, 0)] [] in dist --selectMovtoCorralVisit ambiente d cor (n * m + 1) (-1, -1)

testSekctMovToNino :: Ambiente -> ((Int, Int), Int)
testSekctMovToNino ambiente@Ambiente {robots = Robot rob, corral = Corral cor, dimetions = (n, m)} = let dist@(d, f) = bFSCorral ambiente (head rob) [(head rob, 0)] [] [(head rob, 0)] [] in selectMovtoCorralVisit ambiente d cor (n * m + 1) (-1, -1)

getSuciedad :: Ambiente -> Int
getSuciedad ambiente@Ambiente {suciedad = Suciedad suc} = length suc

calculateTotalPosibleSuciedad :: Ambiente -> Int
calculateTotalPosibleSuciedad ambiente@Ambiente {dimetions = (n, m), obstaculos = Obstaculo obs, ninos = Ninos ni, robots = Robot rob} =
  let total = n * m
      cantObs = length obs
      repNinos = countRepNinos ambiente ni
      cantAgents = countRepRobot ambiente rob
   in total - cantObs - repNinos - cantAgents

countRepNinos :: Ambiente -> [(Int, Int)] -> Int
countRepNinos _ [] = 0
countRepNinos ambiente ninos@(n : ns) = if isNinosInCorral ambiente n then 1 + countRepNinos ambiente ns else 2 + countRepNinos ambiente ns

countRepRobot :: Ambiente -> [(Int, Int)] -> Int
countRepRobot _ [] = 0
countRepRobot ambiente robots@(r : rs) = if isRobotInCorral ambiente r then countRepNinos ambiente rs else 1 + countRepRobot ambiente rs

calculatePorcentSuciedad :: Ambiente -> Double
calculatePorcentSuciedad ambiente@Ambiente {suciedad = Suciedad suc} =
  let total = calculateTotalPosibleSuciedad ambiente
      cantSuciedad = length suc
   in realToFrac (cantSuciedad * 100) / realToFrac total