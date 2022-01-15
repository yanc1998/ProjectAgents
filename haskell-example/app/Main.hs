module Main where

import Ambiente.Ambiente (test, test2, test3, testMovObst,testMOveAllChildren,generateAmbiente)
import System.Random (getStdGen, newStdGen)
import Utils (adyacentesPos, createSquare, randomDirections, randomNumber, testRandom)

main :: IO ()
main = do
  g <- newStdGen
  r <- testRandom (head (randomDirections g))
  g <- newStdGen
  ee <- testRandom (head (randomDirections g))
  let e = test2 in print e
  let ady = adyacentesPos (1, 1) in print ady
  let sc = createSquare (2, 2) 6 6 3 [] in print sc
  let tt = test3 in print tt
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g <- newStdGen
  let test = testMOveAllChildren g in print test
  g1 <- newStdGen
  g2 <- newStdGen
  let amb = generateAmbiente 7 6 5 3 4 1 g1 g2 in print amb

  
  

