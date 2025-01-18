module Example where

import qualified Data.Map as Map
import Bacus
import Bacus.Types
import Bacus.Event (Primitive(..), Event(..))

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

exampleEvents :: [Event]
exampleEvents = [
    -- Chart of accounts
    Chart (Add Asset "cash"),
    Chart (Add Equity "equity"),
    Chart (Add Equity "re"),
    Chart (Add Income "sales"),
    Chart (Offset "sales" "refunds"),
    Chart (Add Expense "salaries"),
    -- Business entries
    PostDouble "cash" "equity" 10,
    PostDouble "cash" "sales" 15,
    PostDouble "refunds" "cash" 3,
    PostDouble "salaries" "cash" 7,
    -- Make ledger copy before close
    Unsafe [PCopy],
    -- Closing entries
    PostDouble "sales" "refunds" 3,
    PostDouble "sales" "re" 12,
    PostDouble "re" "salaries" 7,
    -- Discard temporary accounts from ledger
    Unsafe (map PDrop ["sales", "refunds", "salaries"])
    ]

exampleStream2 :: [Event]
exampleStream2 =
  [ Chart (Add Asset "cash"),
    Chart (Add Equity "equity"),
    Chart (Add Equity "re"),
    Chart (Add Income "sales"),
    Chart (Offset "sales" "refunds"),
    PostDouble "cash" "equity" 33,
    PostDouble "cash" "sales" 500,
    PostDouble "refunds" "cash" 100,
    Close "re"
  ]
