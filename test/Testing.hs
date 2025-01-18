module Main (main) where

import Bacus
import Bacus.Types
import Bacus.Event
import qualified Example as E
import Test.HUnit
import Data.Either (isRight)

testStreamBalances :: Test
testStreamBalances = TestCase $ do
  let (errs, book) = runP (concat E.exampleStream)
  let testBalances = toBalances $ ledgerB book
  if isRight errs
    then assertEqual "Balances are equal" E.exampleBalances testBalances
    else assertFailure $ "Error: " ++ show errs

main :: IO ()
main = do
  _ <- runTestTT testStreamBalances
  return ()
