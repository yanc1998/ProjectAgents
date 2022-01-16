module Main where

import Ambiente.Ambiente (generateAmbiente,moveAllChildren, Ambiente)
import System.Random (getStdGen, newStdGen)
import Utils (adyacentesPos, createSquare, randomNumber)

main :: IO ()
main = do
  g1 <- newStdGen
  g2 <- newStdGen
  let amb = generateAmbiente 7 6 5 3 4 1 g1 g2
  print amb
  simulate amb 3
  
--Simualar el movimiento del ambiente
simulate:: Ambiente -> Int -> IO()
simulate ambiente 0 = print ambiente
simulate ambiente c = do
  g <- newStdGen
  let newAmb = moveAllChildren ambiente g
  simulate newAmb (c-1)


  
  

