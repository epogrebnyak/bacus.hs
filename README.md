# bacus.hs
`bacus` is a minimal single-entry accounting ledger that is fully controlled by a sequence of events.

`bacus` demonstrates a rather complete bookkeeping system that operates on 5 types of primitive commands applied to the ledger (add, offset, post, copy, and drop). More traditional compound commands (double, multiple, close) can be interpreted in terms of the primitives.

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

## Motivation

The 'real' accounting systems by large incumbent players like Intuit (US), SAP (Germany), 1C (Russia), Tally (India), Xero (Australia) do a lot of work but are also rather difficult to manage.

If we focus on the bookkeeping part alone (no document handling) within one accounting period, we can get to a very, yet complete book-keeping system. Such a system should satisfy the following requirements:

1. Able to specify a chart of accounts.
2. Post accounting entries:
   - single,
   - double, or
   - multiple.
3. Close temporary accounts at the period end, including:
   - create a sequence of account balance transfers;
   - make data for the income statement available before and after closing;
   - block modifications to temporary accounts after closing.
4. Produce financial reports.
5. Save data for the next accounting period.

It is a bit surprising that such a system requires records of events of just five types to be operational.

## Primitives

The state of the ledger is fully determined by a list of primitives: you can re-run the list on an empty ledger to arrive at the same ledger state.

These five types of events, or primitives, are:

1. Add an empty regular account of asset, equity, liability, income, or expense type.
2. Offset a regular account with an empty contra account.
3. Post a single entry to an account.
4. Make a copy of the existing ledger before closing accounts.
5. Drop an empty temporary account from the current ledger.

The primitives also fit well for database storage and serialization.

## Account Closing

At the end of the accounting period, the accounts will close in the following way:

1. Make a copy of the ledger to save data for the income statement — this will preserve income and expense accounts and their contra accounts.
2. Make closing entries for temporary accounts and transfer these account balances to an aggregation account (retained earnings).
3. Drop temporary accounts from the current ledger — this will ensure that post-close entries will affect permanent accounts only (note that the temporary accounts are saved in a ledger copy at step 1).

## Remarks

- Account names are strings that can contain either a mnemonic name or an account code.
- Processing primitives can result in an error, for example, if no specified account exists in the ledger.
