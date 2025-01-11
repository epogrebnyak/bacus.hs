module Main where

import Bacus (runP, runE, exampleStream)
import Bacus.Types (Event)
import Bacus.Print (diagnose) 
-- import Example (exampleStream) 

whatHappened :: [Event] -> IO ()
whatHappened events = do  
  let (errs', book') = runE events
  print errs'
  diagnose book'


main :: IO ()
main =  whatHappened exampleStream
-- do 
--  let (errs, book) =  runP (concat exampleStream)
--  print errs
--  diagnose book

