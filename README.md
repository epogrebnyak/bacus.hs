# bacus.hs

`bacus` is a minimal single entry accounting ledger that is fully controlled by a list of events (primitives) of only 5 types
(add, offset, post, copy and drop). 

## Mimimal example

```haskell
import Bacus (Add, Offset, Post, Copy, Drop, Debit, Credit, T5(..), runP, showTrialBalance, showChart)

prims = [[Add Asset "cash", Add Equity "eq"], [Post Debit "cash" 1000, Post Credit "eq" 1000]]

main :: IO ()
main = do
  case runP concat ps of
    Left errs -> putStrLn $ "Errors: " + show errs
    Right book -> do
       print $ showTrialBalance book
       print $ showChart book
```

## Motivation

<!--
The corporate accounting systems are dominated by large incumbent players like Intuit (US), SAP (Germany), 1C (Russia), Tally (India).
The prices for this software can go down and its quality can improve if there were less barriers to competition  
which may happen if larger part of accounting standards were provided as open source data and code. 
-->

`bacus` aims to demonstrate that a tiny accounting system can satisfy the following requirements:

1. able to specify a chart of accounts;
2. post accounting entries to accounts;
3. properly close temporary accounts to accumulation account at period end;
4. make ledger data available for reports before and after close;
5. protect ledger from invalid changes after closing.

Specifically, all these objectives can be satisfied on a ledger that is controlled by only 5 types of events. 

## Events (primitives)

The state of the ledger is fully determined by a list of primitives: you can re-run the list on empty ledger to arrive to the same ledger state.

These five types of events, or primitives, are:

1. add an empty regular account of asset, equity, liability, income or expense type;
3. offset a regular account with an empty contra account;
4. post a signle entry to an account;
5. make a copy of existing ledger before closing accounts;
6. drop an empty temporary account from current ledger. 

The primitives are well-suited for database storage or serialisation.

## Account closing 

At accounting period end the accounts will close in the following way:

1. make a copy of existing ledger to save data for income statement (this will preserve ledger data for income statement);
2. make closing entries for temporary accounts and transfer these account balances to an aggregation account (retained earning);
3. drop temporary accounts from current ledger (this will ensure post-close entries may affect permanent account only).

Note that the temporary accounts before closing will be persisted in a ledger copy.

## More notes

- Account names are strings that can contain either a mnemonic name or account code.
- Processing primitives can result in an error, for example no specified account in ledger.
