module Bacus where

import Control.Monad.State (State, gets, put, runState)
import qualified Data.Map as Map

type Amount = Int -- This could be Decimal E2

type Name = String

data Side = Debit | Credit deriving (Show, Eq)

data Pair = Pair Name Name deriving (Show, Eq)

data DoubleEntry = DoubleEntry Name Name Amount deriving (Show, Eq)

data SingleEntry = SingleEntry Side Name Amount deriving (Show, Eq)

data TAccount = TAccount Side Amount Amount deriving (Show, Eq)

type AccountMap = Map.Map Name TAccount

type Balances = Map.Map Name Amount

data T5 = Asset | Expense | Equity | Liability | Income deriving (Show, Eq)

data Role = Regular T5 | Contra Name deriving (Show, Eq)

type ChartMap = Map.Map Name Role

data Book = Book {chartB :: ChartMap, ledgerB :: AccountMap, copyB :: Maybe AccountMap} deriving (Show)

data ChartItem = Add T5 Name | Offset Name Name

data Primitive = PAdd T5 Name | POffset Name Name | PPost Side Name Amount | PDrop Name | PCopy

data Error = NotFound Name | AlreadyExists Name | NotRegular Name | NotZero Name deriving (Show)

includes :: Map.Map Name b -> Name -> Bool
someMap `includes` name = Map.member name someMap

accountBalance :: TAccount -> Amount
accountBalance (TAccount Debit a b) = a - b
accountBalance (TAccount Credit a b) = b - a

toBalances :: AccountMap -> Balances
toBalances accMap = accountBalance <$> accMap

isEmpty :: TAccount -> Bool
isEmpty tA = 0 == accountBalance tA

debitAccount :: Amount -> TAccount
debitAccount b = TAccount Debit b 0

creditAccount :: Amount -> TAccount
creditAccount = TAccount Credit 0

debit :: Name -> Amount -> SingleEntry
debit = SingleEntry Debit

credit :: Name -> Amount -> SingleEntry
credit = SingleEntry Credit

-- Create AccountMap from ChartMap
toAccountMap :: ChartMap -> AccountMap
toAccountMap chartMap = Map.fromList $ do
  name <- Map.keys chartMap
  Just side <- [whichSide name chartMap]
  return (name, emptyAccount side)

-- Create book
fromChartMap :: ChartMap -> Book
fromChartMap chartMap = Book chartMap (toAccountMap chartMap) Nothing

-- Create empty book
emptyBook :: Book
emptyBook = fromChartMap Map.empty

-- Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = f Debit == f Credit
  where
    f = sideSum posts

-- Sum amounts of all entries of a given side (debit or credit)
sideSum :: [SingleEntry] -> Side -> Amount
sideSum posts side = sum [a | SingleEntry s _ a <- posts, s == side]

-- Post single entry to t-account
postS :: Side -> Amount -> TAccount -> TAccount
postS Debit amount (TAccount side d c) = TAccount side (d + amount) c
postS Credit amount (TAccount side d c) = TAccount side d (c + amount)

createAccount :: Side -> Amount -> TAccount
createAccount Debit b = TAccount Debit b 0
createAccount Credit b = TAccount Credit 0 b

-- Create an empty T-account
emptyAccount :: Side -> TAccount
emptyAccount side = TAccount side 0 0

net :: TAccount -> TAccount
net tA@(TAccount side _ _) = createAccount side (accountBalance tA)

-- Determine debit or credit side for a T5 account type
which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

-- Get side of a given account
whichSide :: Name -> ChartMap -> Maybe Side
whichSide name chartMap = case Map.lookup name chartMap of
  Just (Regular t) -> Just (which t)
  Just (Contra cname) -> toggle <$> whichSide cname chartMap
  Nothing -> Nothing

-- Reverse side
toggle :: Side -> Side
toggle Debit = Credit
toggle Credit = Debit

updateAdd :: ChartMap -> AccountMap -> T5 -> Name -> (ChartMap, AccountMap)
updateAdd chartMap accMap t name =
  let chartMap' = Map.insert name (Regular t) chartMap
      accMap' = case whichSide name chartMap' of
        Just side -> Map.insert name (emptyAccount side) accMap
        Nothing -> accMap
   in (chartMap', accMap')

updateOffset :: ChartMap -> AccountMap -> Name -> Name -> (ChartMap, AccountMap)
updateOffset chartMap accMap name contraName =
  let chartMap' = Map.insert contraName (Contra name) chartMap
      accMap' = case whichSide contraName chartMap' of
        Just side -> Map.insert contraName (emptyAccount side) accMap
        Nothing -> accMap
   in (chartMap', accMap')

-- Check if provided names are correct for creating a contra account
eitherAllowed :: ChartMap -> Name -> Name -> Either Error T5
eitherAllowed chartMap name contraName =
  if chartMap `includes` contraName
    then Left (AlreadyExists contraName)
    else case Map.lookup name chartMap of
      Just (Regular t) -> Right t
      Just _ -> Left $ NotRegular name
      Nothing -> Left $ NotFound name

update :: Primitive -> State Book (Either Error ())
update p = do
  chart <- gets chartB
  ledger <- gets ledgerB
  copy <- gets copyB
  case p of
    (PAdd t name) ->
      case Map.lookup name chart of
        Just _ -> return $ Left (AlreadyExists name)
        Nothing -> do
          let (chart', ledger') = updateAdd chart ledger t name
          put $ Book chart' ledger' copy
          return $ Right ()
    (POffset name contraName) ->
      case eitherAllowed chart name contraName of
        Right _ -> do
          let (chart', ledger') = updateOffset chart ledger name contraName
          put $ Book chart' ledger' copy
          return $ Right ()
        Left e -> return $ Left e
    (PPost side name amount) ->
      if not (ledger `includes` name)
        then return $ Left (NotFound name)
        else do
          let tAccount' = postS side amount (ledger Map.! name)
          let ledger' = Map.insert name tAccount' ledger
          put $ Book chart ledger' copy
          return $ Right ()
    PDrop name ->
      case Map.lookup name ledger of
        Just tAccount ->
          if isEmpty tAccount
            then do
              let ledger' = Map.delete name ledger
              put $ Book chart ledger' copy
              return $ Right ()
            else return $ Left (NotZero name)
        Nothing -> return $ Left (NotFound name)
    PCopy -> do
      put $ Book chart ledger (Just ledger)
      return $ Right ()

foldPrimitives :: Book -> [Primitive] -> ([Either Error ()], Book)
foldPrimitives b prims = runState (mapM update prims) b

runP :: [Primitive] -> ([Either Error ()], Book)
runP = foldPrimitives emptyBook
