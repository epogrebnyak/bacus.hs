# bacus.hs

`bacus` is a minimal accounting ledger that is fully controlled by a sequence of events that modify 
a chart of accounts or account balances in ledger. 
These events are either primitive commands 
or compound commands that can be expressed in terms of primitives.

`bacus` aims to demonstrate that operations needed for the book-keeping cycle can be expressed 
in a grammar of just five verbs (add, offset, post, drop, copy), thus making `bacus` a kind of 
an assembly language for accounting.

Earlier attempts include a similar Python project.

## Minimal example

```haskell
import Bacus
import Bacus.Print

prims = [[PAdd Asset "cash", PAdd Asset "inv", PAdd Equity "eq"], 
         [PPost Debit "cash" 1000, PPost Credit "eq" 1000],
         [PPost Debit "inv" 250, PPost Credit "cash" 250]]

main :: IO ()
main = do
  let (errors, book) = runP $ concat prims
  print errors
  putStrLn $ showChart book "Chart of accounts"
  putStrLn $ showTrialBalance book "Trial balance"
  putStrLn $ showBalances book "Account balances" 
```

## Requirements 

A small yet complete book-keeping system should satisfy the following requirements:

1. Able to specify a chart of accounts.
2. Post accounting entries (single, double or multiple) 
3. Close temporary accounts at the period end:
   - create a sequence of account balance transfers;
   - make data for the income statement available before and after closing;
   - block modifications to temporary accounts after closing.
4. Produce financial reports.
5. Save data for the next accounting period.

`bacus` satisfies these requirements except for reporting - there is no operation classification for the cash flow statement.

## Primitives

The state of the ledger is fully determined by a list of primitives: you can re-run the list on an empty ledger to arrive at the same ledger state.

These five types of events, or primitives, are:

1. Add an empty regular account of asset, equity, liability, income, or expense type.
2. Offset a regular account with an empty contra account.
3. Post a single entry to an account.
4. Make a copy of the existing ledger before closing accounts.
5. Drop an empty temporary account from the current ledger.

It is a bit surprising that such a book-keeping requires just these types of events to be operational. The primitives also fit well for database storage and serialization.

## Compound events

The following events can be expressed in from of a list of primitives:

- double entry;
- multiple entry;
- transfer account balance from one account to another;
- close temporary accounts and transfer retained earnings to accumulation account.  

## Account closing

At the end of the accounting period, the accounts will close in the following order.

1. Make a copy of the ledger to save data for the income statement — this will preserve income and expense accounts and their contra accounts for the income statement.
2. Make closing entries for temporary accounts and transfer account balances to an income summary or aggregation account (retained earnings).
3. Drop temporary accounts from the current ledger — this will ensure that post-close entries will affect permanent accounts only. Note that the temporary accounts are saved in a ledger copy at step 1.

## Remarks

- Account names are strings that can contain either a mnemonic name or an account code.
- Processing primitives can result in an error, for example, if no specified account exists in the ledger.
