module Bacus.Print where

import Bacus
import qualified Data.Map as Map
import Data.Either (isLeft)

showTrialBalanceGross :: Book -> String -> String
showTrialBalanceGross = fromB id showTAccount

showTrialBalance :: Book -> String -> String
showTrialBalance = fromB net showTAccount

showTAccount :: (Name, TAccount) -> String
showTAccount (name, TAccount _ a b) = name ++ ": " ++ show a ++ " " ++ show b

fromB :: (TAccount -> a) -> ((Name, a) -> String) -> Book -> String -> String
fromB f g b t = unlines $ t : (two . g <$> Map.toList (f <$> ledgerB b))

showBalances :: Book -> String -> String
showBalances = fromB accountBalance (\(name, b) -> name ++ ": " ++ show b)

showRole :: Name -> Role -> String
showRole name (Regular t) = name ++ ": " ++ show t
showRole name (Contra n) = name ++ ": " ++ "contra account to " ++ n

showChart :: Book -> String -> String
showChart b t = unlines $ t : (two . uncurry showRole <$> Map.toList (chartB b))

two :: [Char] -> [Char]
two = (++) "  "

revealErrors :: [Either Error ()] -> IO ()
revealErrors errs = do
  putStrLn $ unlines $ "The following events were discarded:" : (show <$> filter isLeft errs)

diagnose :: Book -> IO ()
diagnose b = do
  putStrLn $ showChart b "Chart of accounts:"
  putStrLn $ showTrialBalanceGross b "T-accounts:"
  putStrLn $ showTrialBalance b "Trial balance (net):"
  putStrLn $ showBalances b "Account balances:"
