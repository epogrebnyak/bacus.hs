module Bacus.Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import qualified Data.Map as Map

-- | Amount for monetary value (could be implemented as Decimal E2)
type Amount = Int

-- | String identifier for accounts
type Name = String

-- | Left or right side of a T-account
data Side = Debit | Credit deriving (Show, Eq)

-- | Two account names
data Pair = Pair Name Name deriving (Show, Eq)

-- | Transaction that will debit one account and credit another with the same amount
data DoubleEntry = DoubleEntry Name Name Amount deriving (Show, Eq)

-- | Transaction with a side, account name and amount
data SingleEntry = SingleEntry Side Name Amount deriving (Show, Eq)

-- | T-account has a balancing side, debit balance and credit balance
data TAccount = TAccount Side Amount Amount deriving (Show, Eq)

-- | Map account names to T-accounts
type AccountMap = Map.Map Name TAccount

-- | Map account names to account balances
type Balances = Map.Map Name Amount

-- | Five types of regular accounts
data T5 = Asset | Expense | Equity | Liability | Income deriving (Show, Eq)

-- | Represents an account's role as either regular or a contra account
data Role = Regular T5 | Contra Name deriving (Show, Eq)

-- | Maps account names to their roles
type ChartMap = Map.Map Name Role

-- | Book contains a chart of accounts, ledger and a ledger copy after close
data Book = Book {chartB :: ChartMap, ledgerB :: AccountMap, copyB :: Maybe AccountMap} deriving (Show)

-- | Operations for adding accounts to chart
data ChartItem = Add T5 Name | Offset Name Name

-- | Possible error conditions
data Error
  = NotFound Name
  | AlreadyExists Name
  | NotRegular Name
  | NotEquity Name
  | NotZero Name
  | NotBalanced [SingleEntry]

-- | Error messages
instance Show Error where
  show (NotFound name) = "Account not found: " ++ name
  show (AlreadyExists name) = "Account already exists: " ++ name
  show (NotRegular name) = "Account is not regular: " ++ name
  show (NotEquity name) = "Equity account required: " ++ name
  show (NotZero name) = "Account is not zero: " ++ name
  show (NotBalanced entries) = "Entries not balanced: " ++ show entries
