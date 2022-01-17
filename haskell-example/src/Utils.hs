module Utils
  ( isContain,
    adyacentesPos,
    isValidPos,
    randomNumber,
    createSquare,
  )
where

import System.Random (Random (randomRs), StdGen)

--resive un entero y devuelve una lista infinita de enteros entre 0 y el numero de entrada
randomNumber :: Int -> StdGen -> [Int]
randomNumber n = randomRs (0, n)

--resive una lista de elementos y un elemento y retorna verdadero si el elemento esta contenido en la lista
isContain :: (Eq a) => [a] -> a -> Bool
isContain [] _ = False
isContain (x : xs) value = (x == value) || isContain xs value

--las 4 direcciones del movimineto de los ninos
directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (-1, 0), (0, -1)]

--las 8 direcciones del movimineto para crear el cuadrado del corral
directionsToSquare :: [(Int, Int)]
directionsToSquare = directions ++ [(1, -1), (-1, 1), (1, 1), (-1, -1)]

--resive una posicion y devuelve la lista de las posiciones andyacentes a esta (para las 4 direcciones del movimiento del nino)
adyacentesPos :: (Int, Int) -> [(Int, Int)]
adyacentesPos (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

--resive una posicion y devuelve la lista de las posiciones andyacentes a esta (para las 8 direcciones del movimiento para crear el corral)
adyacentesPosToSquare :: (Int, Int) -> [(Int, Int)]
adyacentesPosToSquare (x, y) = [(x + dx, y + dy) | (dx, dy) <- directionsToSquare]

--resive una posicion y las dimenciones del tablero y retorna verdadero si la posicion es valida (esta dentro del tablero)
isValidPos :: (Int, Int) -> Int -> Int -> Bool
isValidPos (x, y) n m = x < n && y < m && x >= 0 && y >= 0

--BFS para crear a partir de una posicion lo mas parecido a un cuadrado con cantidad de casillas igual a c y centro en esa posicion
createSquareVisit :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> Int -> [(Int, Int)]
createSquareVisit [] visit _ _ _ = visit
createSquareVisit _ visit 0 _ _ = visit
createSquareVisit pila@(p : ps) visit c n m = if isContain visit p || not (isValidPos p n m) then createSquareVisit ps visit c n m else createSquareVisit (ps ++ adyacentesPosToSquare p) (p : visit) (c -1) n m

--crear el cuadrado a partir de la posicion inicial
createSquare :: (Int, Int) -> Int -> Int -> Int -> [(Int, Int)]
createSquare initPos n m c = createSquareVisit [initPos] [] c n m