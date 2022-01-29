module Main where

import Ambiente.Ambiente (Ambiente, Ninos (Ninos), bFSNinos, calculatePorcentSuciedad, changeAmbiente, generateAmbiente, getPath, getSuciedad, moveAllChildren, robotAgentCargaNino, robotAgentClean, selectMovtoNino, testSekctMovToNino, testSelectMovToCorral)
import System.Random (getStdGen, newStdGen)
import Utils (adyacentesPos, createSquare, randomNumber)

main :: IO ()
main = do
  g1 <- newStdGen
  g2 <- newStdGen
  let amb = generateAmbiente 10 10 7 5 5 1 g1 g2
  print amb

  print "Start test Carga Ninos"
  testCargaNino amb 30 1000 100
  print "Start test Clean"
  testClean amb 30 1000 100

--Simualar el movimiento del ambiente para el agente que primero carga el nino
simulateCargaNino :: Ambiente -> Int -> Int -> Int -> IO ()
simulateCargaNino ambiente 0 _ _ = print (calculatePorcentSuciedad ambiente)
simulateCargaNino ambiente c t at = do
  -- ///testing///////////////////////////

  -- ////movimiento del agente///////////////////
  let ambToRobot = robotAgentCargaNino ambiente
  -- ////////////////////////////////////////////

  -- ///movimiento de los ninos//////////////////
  g <- newStdGen
  let newAmb = moveAllChildren ambToRobot g
  -- ////////////////////////////////////////////

  -- ///// turno de cambiar el ambiente //////////
  g1 <- newStdGen
  g2 <- newStdGen

  if at == t then simulateCargaNino (changeAmbiente newAmb g1 g2) (c -1) t 0 else simulateCargaNino newAmb (c -1) t (at + 1)

--Simualar el movimiento del ambiente para el agente que primero limpia
simulateClean :: Ambiente -> Int -> Int -> Int -> IO ()
simulateClean ambiente 0 _ _ = print (calculatePorcentSuciedad ambiente)
simulateClean ambiente c t at = do
  -- ///testing///////////////////////////

  -- ////movimiento del agente////////////////
  let ambToRobot = robotAgentClean ambiente
  -- /////////////////////////////////////////

  -- ///movimiento de los ninos///////////////
  g <- newStdGen
  let newAmb = moveAllChildren ambToRobot g
  -- /////////////////////////////////////////

  -- ///// turno de cambiar el ambiente ///
  g1 <- newStdGen
  g2 <- newStdGen

  if at == t then simulateClean (changeAmbiente newAmb g1 g2) (c -1) t 0 else simulateClean newAmb (c -1) t (at + 1)

--repetir n veces la simulacion
-- params {ambiente , cantidad de simulaciones, cantiad de turnos de la simulacion,turno de cambio de ambiente }
testCargaNino :: Ambiente -> Int -> Int -> Int -> IO ()
testCargaNino _ 0 n t = print "finish"
testCargaNino ambiente c n t = do
  simulateCargaNino ambiente n t 0
  testClean ambiente (c -1) n t

testClean :: Ambiente -> Int -> Int -> Int -> IO ()
testClean _ 0 n t = print "finish"
testClean ambiente c n t = do
  simulateClean ambiente n t 0
  testClean ambiente (c -1) n t
