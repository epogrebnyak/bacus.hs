module Bacus where

import qualified Data.Map as Map
import Control.Monad.State (State, runState, gets, put)

type Amount = Int  -- This could be Decimal E2
type Name = String
data Side = Debit | Credit deriving (Show, Eq)

data Pair = Pair Name Name deriving (Show, Eq)
data DoubleEntry = DoubleEntry Name Name Amount deriving (Show, Eq)
data SingleEntry = SingleEntry Side Name Amount deriving (Show, Eq)

data TAccount = TAccount Side Amount Amount deriving (Show, Eq)
type AccountMap = Map.Map Name TAccount

data T5 = Asset | Expense | Equity | Liability | Income deriving (Show, Eq)
data Role = Regular T5 | Contra Name deriving (Show, Eq)
type ChartMap = Map.Map Name Role

data Book = Book {chartB :: ChartMap, ledgerB :: AccountMap, copyB :: Maybe AccountMap} deriving (Show)
data ChartItem = Add T5 Name | Offset Name Name
data Primitive = PAccount ChartItem | PPost SingleEntry | PDrop Name | PCopy

data Error = NotFound Name | AlreadyExists Name | NotRegular Name | NotZero Name deriving Show

includes :: Ord a => Map.Map a b -> a -> Bool
someMap `includes` name = Map.member name someMap    

accountBalance :: TAccount -> Amount
accountBalance (TAccount Debit a b) = a - b
accountBalance (TAccount Credit a b) = b - a

debitAccount :: Amount -> TAccount
debitAccount b = TAccount Debit b 0

creditAccount :: Amount -> TAccount
creditAccount b = TAccount Credit 0 b

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
    where f = sideSum posts 

-- Sum amounts of all entries of a given side (debit or credit)
sideSum :: [SingleEntry] -> Side -> Amount
sideSum posts side = sum [a | SingleEntry s _ a <- posts, s == side]

-- Post single entry to t-account
alter :: Side -> Amount -> TAccount -> TAccount
alter Debit amount (TAccount side d c)  = TAccount side (d+amount) c
alter Credit amount (TAccount side d c) = TAccount side d (c+amount)

showLedger :: AccountMap -> [String]
showLedger accountMap = uncurry showTAccount <$> Map.toList accountMap
  where
    showTAccount :: Name -> TAccount -> String
    showTAccount name tAccount = name ++ ": " ++ show (accountBalance tAccount)

-- Create an empty T-account
emptyAccount :: Side -> TAccount
emptyAccount side = TAccount side 0 0

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
    if chartMap `includes` contraName then Left (AlreadyExists contraName) else
        case Map.lookup name chartMap of 
            Just (Regular t) -> Right t
            Just (_) -> Left $ NotRegular name
            Nothing -> Left $ NotFound name 

update :: Primitive -> State Book (Either Error ())
update p = do
  chart <- gets chartB
  ledger <- gets ledgerB
  copy <- gets copyB
  case p of
    PAccount (Add t name) ->
      case Map.lookup name chart of
        Just _ -> return $ Left (AlreadyExists name)
        Nothing -> do
          let (chart', ledger') = updateAdd chart ledger t name
          put $ Book chart' ledger' copy
          return $ Right ()
    PAccount (Offset name contraName) ->
      case eitherAllowed chart name contraName of
        Right _ -> do
          let (chart', ledger') = updateOffset chart ledger name contraName
          put $ Book chart' ledger' copy
          return $ Right ()
        Left e -> return $ Left e
    PPost (SingleEntry side name amount) ->
      if not (ledger `includes` name)
        then return $ Left (NotFound name)
        else do
          let tAccount' = alter side amount (ledger Map.! name)
          let ledger' = Map.insert name tAccount' ledger
          put $ Book chart ledger' copy
          return $ Right ()
    PDrop name ->
      case Map.lookup name ledger of
        Just tAccount -> if accountBalance tAccount /= 0 then 
            return $ Left (NotZero name)
            else do
                let ledger' = Map.delete name ledger
                put $ Book chart ledger' copy
                return $ Right ()
        Nothing -> return $ Left (NotFound name)    
    PCopy -> do
      put $ Book chart ledger (Just ledger)
      return $ Right ()

-- Homemade monadic fold 
-- Need  t0make better use of MapM, Map_, sequence, FoldM
updateBook :: Book -> [Primitive] -> Either Error Book
updateBook book primitives =
    let (result, finalBook) = runState (mapM update primitives) book
    in case sequence result of
        Left err -> Left err
        Right _ -> Right finalBook

runP :: [Primitive] -> Either Error Book
runP = updateBook emptyBook

-- Collect final state from chaining of exampleStreamP    
-- Should return:
--   cash: 15
--   equity: 10
--   re: 5
repl :: IO ()
repl = do
    case runP (concat exampleStreamP) of
        Left err -> putStrLn $ "Error: " ++ show err
        Right book -> mapM_ putStrLn (showLedger (ledgerB book))

exampleStreamP :: [[Primitive]]
exampleStreamP = [
    -- Chart of accounts
    [PAccount $ Add Asset "cash", 
     PAccount $ Add Equity "equity",
     PAccount $ Add Equity "re",
     PAccount $ Add Income "sales",
     PAccount $ Offset "sales" "refunds",
     PAccount $ Add Expense "salaries"],
    -- Business entries 
    [PPost (debit "cash" 10), PPost (credit "equity" 10)],
    [PPost (debit "cash" 15), PPost (credit "sales" 15)],
    [PPost (debit "refunds" 3), PPost (credit "cash" 3)],
    [PPost (debit "salaries" 7), PPost (credit "cash" 7)],
    -- Make ledger copy before close
    [PCopy],
    -- Closing entries
    [PPost (debit "sales" 3), PPost (credit "refunds" 3)],
    [PPost (debit "sales" 12), PPost (credit "re" 12)],
    [PPost (debit "re" 7), PPost (credit "salaries" 7)],
    -- Discard temporary accounts from ledger
    map PDrop ["sales", "refunds", "salaries"]  
    ]

