module Main where

import Bacus (runP)
import Bacus.Print (diagnose) 
import Example (exampleStream) 

main :: IO ()
main = do
  let (errs, book) =  runP (concat exampleStream)
  print errs
  diagnose book
