module Bacus where

import Control.Monad.Except
import Control.Monad.State (State, gets, put, runState)
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

-- | Primitive operations that can be performed on the book
data Primitive = PAdd T5 Name | POffset Name Name | PPost Side Name Amount | PDrop Name | PCopy

-- | Possible error conditions
data Error = NotFound Name | AlreadyExists Name | NotRegular Name | NotZero Name deriving (Show)

type BookOperation a = ExceptT Error (State Book) a

-- Can change to BookOperation [Primitive]
type BookState = BookOperation ()

-- | Check if map includes a given name as a key
includes :: Map.Map Name b -> Name -> Bool
someMap `includes` name = Map.member name someMap

-- | Calculate balance of a T-account
accountBalance :: TAccount -> Amount
accountBalance (TAccount side a b) = case side of
  Debit -> a - b
  Credit -> b - a

-- | Convert ledger to a map of balances
toBalances :: AccountMap -> Balances
toBalances accMap = accountBalance <$> accMap

-- | Checks if a T-account has zero balance
isEmpty :: TAccount -> Bool
isEmpty tA = 0 == accountBalance tA

-- | Create a debit T-account with initial balance
debitAccount :: Amount -> TAccount
debitAccount b = TAccount Debit b 0

-- | Create a credit T-account with initial balance
creditAccount :: Amount -> TAccount
creditAccount = TAccount Credit 0

-- | Create a debit single entry
debit :: Name -> Amount -> SingleEntry
debit = SingleEntry Debit

-- | Create a credit single entry
credit :: Name -> Amount -> SingleEntry
credit = SingleEntry Credit

-- | Create an AccountMap from ChartMap
toAccountMap :: ChartMap -> AccountMap
toAccountMap chartMap = Map.fromList $ do
  name <- Map.keys chartMap
  Just side <- [whichSide name chartMap]
  return (name, emptyAccount side)

-- | Create an empty book from ChartMap
fromChartMap :: ChartMap -> Book
fromChartMap chartMap = Book chartMap (toAccountMap chartMap) Nothing

-- | Create an empty book with no accounts specified
emptyBook :: Book
emptyBook = fromChartMap Map.empty

-- | Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = f Debit == f Credit where f = sideSum posts

-- | Sum amounts of all entries of a given side (debit or credit)
sideSum :: [SingleEntry] -> Side -> Amount
sideSum posts side = sum [a | SingleEntry s _ a <- posts, s == side]

-- | Post single entry to T-account
postS :: Side -> Amount -> TAccount -> TAccount
postS Debit amount (TAccount side d c) = TAccount side (d + amount) c
postS Credit amount (TAccount side d c) = TAccount side d (c + amount)

-- | Create a T-account with an initial balance
createAccount :: Side -> Amount -> TAccount
createAccount Debit b = TAccount Debit b 0
createAccount Credit b = TAccount Credit 0 b

-- | Create an empty T-account
emptyAccount :: Side -> TAccount
emptyAccount side = TAccount side 0 0

-- | Return net balance of a T-account
net :: TAccount -> TAccount
net tA@(TAccount side _ _) = createAccount side (accountBalance tA)

-- | Return normal side for a T5 account type
which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

-- | Get side of a given account
whichSide :: Name -> ChartMap -> Maybe Side
whichSide name chartMap = case Map.lookup name chartMap of
  Just (Regular t) -> Just (which t)
  Just (Contra cname) -> toggle <$> whichSide cname chartMap
  Nothing -> Nothing

-- | Toggles between debit and credit sides
toggle :: Side -> Side
toggle Debit = Credit
toggle Credit = Debit

-- | Update chart and account map when adding a new account
updateAdd :: ChartMap -> AccountMap -> T5 -> Name -> (ChartMap, AccountMap)
updateAdd chartMap accMap t name =
  let chartMap' = Map.insert name (Regular t) chartMap
      accMap' = case whichSide name chartMap' of
        Just side -> Map.insert name (emptyAccount side) accMap
        Nothing -> accMap
   in (chartMap', accMap')

-- | Update chart and account map when adding a contra account
updateOffset :: ChartMap -> AccountMap -> Name -> Name -> (ChartMap, AccountMap)
updateOffset chartMap accMap name contraName =
  let chartMap' = Map.insert contraName (Contra name) chartMap
      accMap' = case whichSide contraName chartMap' of
        Just side -> Map.insert contraName (emptyAccount side) accMap
        Nothing -> accMap
   in (chartMap', accMap')

-- | Validate contra account creation
eitherAllowed :: ChartMap -> Name -> Name -> Either Error T5
eitherAllowed chartMap name contraName =
  if chartMap `includes` contraName
    then Left (AlreadyExists contraName)
    else case Map.lookup name chartMap of
      Just (Regular t) -> Right t
      Just _ -> Left $ NotRegular name
      Nothing -> Left $ NotFound name

--  Create state
createState :: Primitive -> BookState
createState p = do
  chart <- gets chartB
  ledger <- gets ledgerB
  copy <- gets copyB
  case p of
    (PAdd t name) -> do
      when (Map.member name chart) $ throwError (AlreadyExists name)
      let (chart', ledger') = updateAdd chart ledger t name
      put $ Book chart' ledger' copy
    (POffset name contraName) -> do
      -- This is a check eitgherAllowed succeeds
      void $ liftEither $ eitherAllowed chart name contraName
      let (chart', ledger') = updateOffset chart ledger name contraName
      put $ Book chart' ledger' copy
    (PPost side name amount) -> do
      unless (ledger `includes` name) $ throwError (NotFound name)
      let tAccount' = postS side amount (ledger Map.! name)
      let ledger' = Map.insert name tAccount' ledger
      put $ Book chart ledger' copy
    PDrop name -> do
      case Map.lookup name ledger of
        Nothing -> throwError $ NotFound name
        Just tAccount -> do
          unless (isEmpty tAccount) $ throwError (NotZero name)
          let ledger' = Map.delete name ledger
          put $ Book chart ledger' copy
    PCopy -> do
      put $ Book chart ledger (Just ledger)

runBookState :: Book -> BookState -> (Either Error (), Book)
runBookState book operation = runState (runExceptT operation) book

runP :: [Primitive] -> (Either Error (), Book)
runP prims = runBookState emptyBook $ mapM_ createState prims

exampleOperation :: BookOperation ()
exampleOperation = do
  createState (PAdd Asset "cash")
  createState (PAdd Equity "capital")
  createState (PPost Debit "cash" 1000)
  createState (PPost Credit "capital" 1000)
