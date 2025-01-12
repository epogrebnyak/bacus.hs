module Main (main) where

import Bacus
import Bacus.Types
import qualified Example as E
import Test.HUnit

testStreamBalances :: Test
testStreamBalances = TestCase $ do
  let (errs, book) = runP (concat E.exampleStream)
  if not (null errs)
    then assertFailure $ "Error: " ++ show errs
    else assertEqual "Balances are equal" E.exampleBalances (toBalances $ ledgerB book)

main :: IO ()
main = do
  _ <- runTestTT testStreamBalances
  return ()
