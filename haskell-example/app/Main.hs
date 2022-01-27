module Main where

import Ambiente.Ambiente (Ambiente, Ninos (Ninos), bFSNinos, changeAmbiente, generateAmbiente, getPath, moveAllChildren, robotAgent, selectMovtoNino, testSekctMovToNino, testSelectMovToCorral)
import System.Random (getStdGen, newStdGen)
import Utils (adyacentesPos, createSquare, randomNumber)

main :: IO ()
main = do
  g1 <- newStdGen
  g2 <- newStdGen
  let amb = generateAmbiente 7 6 5 3 4 1 g1 g2
  print amb
  g3 <- newStdGen
  g4 <- newStdGen
  let chAmb = changeAmbiente amb g3 g4 in print chAmb

  print " "
  simulate amb 1000 10 0

--Simualar el movimiento del ambiente
simulate :: Ambiente -> Int -> Int -> Int -> IO ()
simulate ambiente 0 _ _ = print ambiente
simulate ambiente c t at = do
  g <- newStdGen
  let newAmb = moveAllChildren ambiente g
  print "Movimiento de los Ninos"
  print newAmb

  let tt = testSelectMovToCorral newAmb
  print "bfs para corral"
  print tt
  let tt2 = testSekctMovToNino newAmb
  print "bfs para ninos"
  print tt2
  print " Movimiento del robot"
  let ambToRobot = robotAgent newAmb
  print ambToRobot
  g1 <- newStdGen
  g2 <- newStdGen
  if at == t then simulate (changeAmbiente ambToRobot g1 g2) (c -1) t 0 else simulate ambToRobot (c -1) t (at + 1)