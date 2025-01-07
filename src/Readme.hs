import Bacus
import Bacus.Print

prims :: [[Primitive]]
prims =
  [ [PAdd Asset "cash", PAdd Asset "inv", PAdd Equity "eq"],
    [PPost Debit "cash" 1000, PPost Credit "eq" 1000],
    [PPost Debit "isnv" 250, PPost Credit "cash" 250],
    -- non-existent account
    [PPost Debit "bank" 1]
  ]

main :: IO ()
main = do
  let (errors, book) = runP $ concat prims
  revealErrors errors
  putStrLn $ showChart book "Chart of accounts"
  putStrLn $ showTrialBalance book "Trial balance"
  putStrLn $ showBalances book "Account balances"
