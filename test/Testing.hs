module Main (main) where

import Test.HUnit
import Bacus
import qualified Data.Map as Map

expectedAccountMap :: Map.Map String Amount
expectedAccountMap = Map.fromList [("cash",15),("equity",10),("re",5)]

eq' :: Map.Map Name TAccount -> Map.Map Name Amount -> Assertion
eq' accountMap b = assertEqual "Balances are equal" (Map.map accountBalance accountMap) b

-- Test case for ledgerB
testLedgerB :: Test
testLedgerB = TestCase $ do
    case runP (concat exampleStreamP) of
        Left err -> assertFailure $ "Error: " ++ show err
        Right book -> eq' (ledgerB book) expectedAccountMap

-- List of all tests
tests :: Test
tests = TestList [testLedgerB]

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
