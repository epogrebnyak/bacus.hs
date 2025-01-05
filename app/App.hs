module Main where

import qualified Bacus (repl) 

main :: IO ()
main = do
  putStrLn "Attempting result:"
  Bacus.repl
