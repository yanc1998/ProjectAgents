module Cp3
  ( minimo,
    pertenece,
    ABB (..),
  )
where

data ABB = ABB {valor :: Int, hi :: ABB, hd :: ABB} | Nil deriving (Eq)

minimo :: ABB -> Int
minimo Nil = 0
minimo abb@ABB {hi = i, valor = v} = if i == Nil then v else minimo i

pertenece :: ABB -> Int -> Bool
pertenece Nil _ = False
pertenece abb@ABB {hi = i, hd = d, valor = v} x
  | x > v = pertenece d x
  | x < v = pertenece i x
  | otherwise = True