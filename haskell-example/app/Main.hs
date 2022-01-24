module Main where

import Ambiente.Ambiente (Ambiente, Ninos (Ninos), bFSNinos, generateAmbiente, moveAllChildren,getPath, selectMovtoNino,robotAgent,testSelectMovToCorral)
import System.Random (getStdGen, newStdGen)
import Utils (adyacentesPos, createSquare, randomNumber)

main :: IO ()
main = do
  g1 <- newStdGen
  g2 <- newStdGen
  let amb = generateAmbiente 7 6 5 3 4 1 g1 g2
  let tt = testSelectMovToCorral amb
  print tt
  print amb
  print " "
  simulate amb 30

--Simualar el movimiento del ambiente
simulate :: Ambiente -> Int -> IO ()
simulate ambiente 0 = print ambiente
simulate ambiente c = do
  g <- newStdGen
  let newAmb = moveAllChildren ambiente g
  print "Movimiento de los Ninos"
  print newAmb
  
  print " Movimiento del robot"
  let tt = testSelectMovToCorral newAmb in print tt
  let ambToRobot = robotAgent newAmb
  print ambToRobot
  
  
  simulate ambToRobot (c -1)
