module Main where

import Ambiente.Ambiente (test2,test,test3,testMovObst)

import Utils (randomNumber, randomPos, randomPos2,adyacentesPos,createSquare)

main :: IO ()
main = do
  a <- randomNumber 4
  b <- randomNumber 4
  f <- randomNumber 5
  g <- randomNumber 5
  let c = randomPos a b in print c
  let d = randomPos f g in print d
  let e = test2 in print e
  let ady = adyacentesPos (1,1) in print ady
  let sc = createSquare (2,2) 6 6 3 [] in print sc
  let tt = test3 in print tt
  let testO = testMovObst in print testO
