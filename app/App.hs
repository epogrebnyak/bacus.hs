module Main where

import Bacus.Event (Event, runP, runE)
import Example (exampleStream2)
import Bacus.Print (diagnose) 

whatHappened :: [Event] -> IO ()
whatHappened events = do  
  let (errs', book') = runE events
  print errs'
  diagnose book'


main :: IO ()
main =  whatHappened exampleStream2
-- do 
--  let (errs, book) =  runP (concat exampleStream)
--  print errs
--  diagnose book

