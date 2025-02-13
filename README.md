# bacus.hs

[![Haskell](https://github.com/epogrebnyak/bacus.hs/actions/workflows/haskell.yml/badge.svg)](https://github.com/epogrebnyak/bacus.hs/actions/workflows/haskell.yml)

`bacus` is an experimental accounting ledger fully controlled by a sequence of events
that change chart of accounts, post accounting entries and close temporary accounts
at period end.

`bacus` aims to demonstrate that a grammar of just five verbs (add, offset, post, drop, copy)
is enough to express the primitive operations needed for the bookkeeping cycle,
making `bacus` akin to an assembly language for accounting.

Earlier attempts include
[a similar project implemented in Python](https://github.com/epogrebnyak/abacus-minimal)
that helped to shape the logic of a minimal ledger.

## Minimal example

```haskell
import Bacus
import Bacus.Print

prims = [[PAdd Asset "cash",
          PAdd Asset "inventory",
          PAdd Equity "equity"],
         [PPost Debit "cash" 1000,     PPost Credit "equity" 1000],
         [PPost Debit "inventory" 250, PPost Credit "cash" 250]]

main :: IO ()
main = do
  let (errors, book) = runP $ concat prims
  print errors
  putStrLn $ showChart book "Chart of accounts"
  putStrLn $ showTrialBalance book "Trial balance"
  putStrLn $ showBalances book "Account balances"
```

## Requirements

A book-keeping system should satisfy the following minimal requirements:

1. Able to specify a chart of accounts.
2. Post accounting entries (single, double or multiple)
3. Close temporary accounts at the period end:
   - create a sequence of account balance transfers;
   - make data for the income statement available before and after closing;
   - disallow chnaging temporary accounts after closing.
4. Produce financial reports.
5. Save data for the next accounting period.

`bacus` satisfies these requirements except for the reporting part 
(no operation classification for the cash flow statement).

## Primitive events

The events in `bacus` are either primitive commands or compound commands
built from these primitives.

The state of the ledger is fully determined by a list of primitive commands:
you can re-run the list on an empty ledger and arrive at the same ledger state.

The five types of primitive commands are:

1. Add an empty regular account of asset, equity, liability, income, or expense type.
2. Offset a regular account with an empty contra account.
3. Post a single entry to an account.
4. Make a copy of the existing ledger before closing temporary accounts.
5. Drop an empty temporary account from the current ledger.

The primitives also fit well for database storage and serialization.

## Compound events

The following events can be expressed as primitives:

- posting double or multiple entry,
- transfer account balance from one account to another;
- close temporary accounts and transfer balances to retained earnings.

## Account closing

At the end of the accounting period, the accounts will close in the following order.

1. Make a copy of the ledger to save data for the income statement — this will preserve income and expense accounts and their contra accounts for the income statement.

2. Make closing entries for temporary accounts and transfer account balances to an income summary account (retained earnings).

3. Drop temporary accounts from the current ledger to ensure that post-close entries will affect permanent accounts only. Note that the temporary accounts are saved in a ledger copy at step 1.

## Remarks

- Account names are strings that can contain either a mnemonic name or an account code.
- Processing primitives may result in an error, for example, if no specified account exists in the ledger.
