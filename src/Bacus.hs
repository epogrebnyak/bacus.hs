module Bacus where

import Control.Monad.Except
import Control.Monad.State (State, gets, put, runState)
import qualified Data.Map as Map

import Bacus.Types
import Bacus.Closing (closingPairs)

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
createState :: Primitive -> BookOperation Primitive
createState p = do
  chart <- gets chartB
  ledger <- gets ledgerB
  copy <- gets copyB
  case p of
    (PAdd t name) -> do
      when (Map.member name chart) $ throwError (AlreadyExists name)
      let (chart', ledger') = updateAdd chart ledger t name
      put $ Book chart' ledger' copy
      return p
    (POffset name contraName) -> do
      -- This is a check eitgherAllowed succeeds
      void $ liftEither $ eitherAllowed chart name contraName
      let (chart', ledger') = updateOffset chart ledger name contraName
      put $ Book chart' ledger' copy
      return p
    (PPost side name amount) -> do
      unless (ledger `includes` name) $ throwError (NotFound name)
      let tAccount' = postS side amount (ledger Map.! name)
      let ledger' = Map.insert name tAccount' ledger
      put $ Book chart ledger' copy
      return p
    PDrop name -> do
      case Map.lookup name ledger of
        Nothing -> throwError $ NotFound name
        Just tAccount -> do
          unless (isEmpty tAccount) $ throwError (NotZero name)
          let ledger' = Map.delete name ledger
          put $ Book chart ledger' copy
          return p
    PCopy -> do
      put $ Book chart ledger (Just ledger)
      return p

runBookState :: BookState -> Book -> (Either Error [Primitive], Book)
runBookState operation = runState (runExceptT operation)

runP :: [Primitive] -> (Either Error [Primitive], Book)
runP prims = runBookState (mapM createState prims) emptyBook

exampleOperation :: BookOperation Primitive
exampleOperation = do
  _ <- createState (PAdd Asset "cash")
  _ <- createState (PAdd Equity "capital")
  _ <- createState (PPost Debit "cash" 1000)
  createState (PPost Credit "capital" 1000)

mkState :: Event -> BookState
mkState (Unsafe ps) = mapM createState ps
mkState (Chart (Add t n)) = do
  p <- createState (PAdd t n)
  return [p]
mkState (Chart (Offset n c)) = do
  p <- createState (POffset n c)
  return [p]
mkState (PostDouble d c a) = mkState $ PostMultiple [debit d a, credit c a]
mkState (PostMultiple posts)
  | isBalanced posts = mapM (\(SingleEntry s n a) -> createState (PPost s n a)) posts
  | otherwise = throwError $ NotBalanced posts
mkState (Transfer from to) = do
  ledger <- gets ledgerB 
  case transfer from to ledger of
    Right (DoubleEntry d c a) -> mkState (PostDouble d c a)
    Left errs -> throwError $ head errs
mkState (Close accName) = do
   chartMap <- gets chartB 
   case Map.lookup accName chartMap of
        Just (Regular Equity) -> mkStateMany $ anyClose chartMap accName
        Just _                -> throwError $ NotEquity accName        
        Nothing               -> throwError $ NotFound accName
  where 
    anyClose :: ChartMap -> Name -> [Event]
    anyClose chartMap accName = concatMap fromPair (closingPairs chartMap accName) 
    fromPair :: Pair -> [Event]
    fromPair (Pair a b) = [Transfer a b, Unsafe [PDrop a]]

-- Transfer balance between accounts
transfer :: Name -> Name -> AccountMap -> Either [Error] DoubleEntry
transfer fromName toName accountMap = let lookup' n = Map.lookup n accountMap in 
    case (lookup' fromName, lookup' toName) of
        (Just tAcc@(TAccount side _ _), Just _) -> 
            Right (transferEntry side fromName toName (accountBalance tAcc))
        (Just _, Nothing) -> Left [NotFound toName]
        (Nothing, Just _) -> Left [NotFound fromName]
        (_, _) -> Left [NotFound toName, NotFound fromName]
    where
        transferEntry :: Side -> Name -> Name -> Amount -> DoubleEntry
        transferEntry Debit from to = DoubleEntry to from
        transferEntry Credit from to = DoubleEntry from to

mkStateMany :: [Event] -> BookState
mkStateMany events = concat <$> mapM mkState events

exampleStream :: [Event]
exampleStream = [
  Chart (Add Asset "cash"),
  Chart (Add Equity "equity"),
  Chart (Add Equity "re"),
  Chart (Add Income "sales"),
  Chart (Offset "sales" "refunds"),
  PostDouble "cash" "equity" 33,
  PostDouble "cash" "sales" 500,
  PostDouble "refunds" "cash" 100,
  Close "re"]

runE :: [Event] -> (Either Error [Primitive], Book)
runE events = runBookState (concat <$> mapM mkState events) emptyBook

