module Example where

import qualified Data.Map as Map
import Bacus

exampleBalances :: Balances
exampleBalances = Map.fromList [("cash", 15), ("equity", 10), ("re", 5)]

exampleStream :: [[Primitive]]
exampleStream = [
    -- Chart of accounts
    [PAdd Asset "cash", 
     PAdd Equity "equity",
     PAdd Equity "re",
     PAdd Income "sales",
     POffset "sales" "refunds",
     PAdd Expense "salaries"],
    -- Business entries 
    [PPost Debit "cash" 10, PPost Credit "equity" 10],
    [PPost Debit "cash" 15, PPost Credit "sales" 15],
    [PPost Debit "refunds" 3, PPost Credit "cash" 3],
    [PPost Debit "salaries" 7, PPost Credit "cash" 7],
    -- Make ledger copy before close
    [PCopy],
    -- Closing entries
    [PPost Debit "sales" 3, PPost Credit "refunds" 3],
    [PPost Debit "sales" 12, PPost Credit "re" 12],
    [PPost Debit "re" 7, PPost Credit "salaries" 7],
    -- Discard temporary accounts from ledger
    map PDrop ["sales", "refunds", "salaries"]  
    ]
