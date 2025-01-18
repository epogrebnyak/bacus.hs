module Bacus where

import Bacus.Types
import qualified Data.Map as Map

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

-- | Return new T-account with net balance
net :: TAccount -> TAccount
net tAccount@(TAccount side _ _) = createAccount side (accountBalance tAccount)

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

-- Transfer balance from one account to another
transfer :: Name -> Name -> AccountMap -> Either [Error] DoubleEntry
transfer fromName toName accountMap =
  let lookup' n = Map.lookup n accountMap
   in case (lookup' fromName, lookup' toName) of
        (Just tAcc@(TAccount side _ _), Just _) ->
          Right (transferEntry side fromName toName (accountBalance tAcc))
        (Just _, Nothing) -> Left [NotFound toName]
        (Nothing, Just _) -> Left [NotFound fromName]
        (_, _) -> Left [NotFound toName, NotFound fromName]
  where
    transferEntry :: Side -> Name -> Name -> Amount -> DoubleEntry
    transferEntry Debit from to = DoubleEntry to from
    transferEntry Credit from to = DoubleEntry from to

-- List all accounts from chart by specific type
byType :: ChartMap -> T5 -> [Name]
byType chartMap t = Map.keys $ Map.filter (== Regular t) chartMap

-- Return list of contra accounts for a specific regular account 
contras :: ChartMap -> Name -> [Name] 
contras chartMap name =
    case Map.lookup name chartMap of
        Just (Regular _) -> Map.keys $ Map.filter (== Contra name) chartMap
        _ -> []

-- Create a tuple with closeTo as second item
toPair :: Name -> Name -> Pair
toPair closeTo name = Pair name closeTo

-- Return all contra accounts for a specific account type
contraPairs :: ChartMap -> T5 -> [Pair]
contraPairs chartMap t = concatMap 
      (\name -> toPair name <$> contras chartMap name) 
      (byType chartMap t)

-- Return closing pairs for regular account that close to accumulation account
accumulationPairs :: ChartMap -> T5 -> Name -> [Pair]
accumulationPairs chartMap t accName = toPair accName <$> byType chartMap t

-- Combine contraPairs and accumulationPairs
allPairs :: ChartMap -> Name -> T5 -> [Pair]
allPairs chartMap accName t = contraPairs chartMap t ++ accumulationPairs chartMap t accName

-- Create complete list of pairs for closing temporary accounts
closingPairs :: ChartMap -> Name -> [Pair]
closingPairs chartMap accName = [Expense, Income] >>= allPairs chartMap accName
