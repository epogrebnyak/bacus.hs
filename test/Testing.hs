module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Bacus
import Bacus.Types
import Bacus.Event
import qualified Example as E
import Data.Either (isRight)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" 
  [ balanceTests
  , emptyBookTests
  ]

balanceTests :: TestTree
balanceTests = testGroup "Balance Tests"
  [ testCase "Stream balances match example" $ do
      let (errs, book) = runP (concat E.exampleStream)
      let testBalances = toBalances $ ledgerB book
      if isRight errs
        then E.exampleBalances @=? testBalances
        else assertFailure $ "Error: " ++ show errs
  ]

emptyBookTests :: TestTree
emptyBookTests = testGroup "Empty Book Tests"
  [ testCase "Empty stream produces empty balances" $ do
      let (errs, book) = runP []
      let testBalances = toBalances $ ledgerB book
      if isRight errs
        then mempty @=? testBalances
        else assertFailure $ "Error: " ++ show errs
  ]
