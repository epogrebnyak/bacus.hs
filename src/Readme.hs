import Bacus
import Bacus.Print
import Bacus.Types

prims :: [Primitive]
prims =
  [ PAdd Asset "cash",
    PAdd Equity "equity",
    PPost Debit "cash" 1000,
    PPost Credit "equity" 1000
  ]

main :: IO ()
main = do
  let (errors, book) = runP prims
  either print (const (pure ())) errors
  putStrLn $ showChart book "Chart of accounts"
  putStrLn $ showTrialBalance book "Trial balance"
  putStrLn $ showBalances book "Account balances"
