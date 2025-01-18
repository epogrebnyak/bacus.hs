module Bacus.Event where

import Bacus
import Bacus.Types
import Control.Monad.Except
import Control.Monad.State (gets, put, runState, State)
import qualified Data.Map as Map

-- | Primitive operations that can be performed on the book
data Primitive = PAdd T5 Name 
               | POffset Name Name 
               | PPost Side Name Amount 
               | PDrop Name 
               | PCopy
               deriving Show

data Event
  = Chart ChartItem
  | PostDouble Name Name Amount
  | PostMultiple [SingleEntry]
  | Transfer Name Name
  | Close Name
  | Unsafe [Primitive]

type BookOperation a = ExceptT Error (State Book) a

type BookState = BookOperation [Primitive]

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

-- https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Except.html#g:2
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
    Just _ -> throwError $ NotEquity accName
    Nothing -> throwError $ NotFound accName
  where
    anyClose :: ChartMap -> Name -> [Event]
    anyClose chartMap accName' = concatMap fromPair (closingPairs chartMap accName')
    fromPair :: Pair -> [Event]
    fromPair (Pair a b) = [Transfer a b, Unsafe [PDrop a]]


mkStateMany :: [Event] -> BookState
mkStateMany events = concat <$> mapM mkState events

runE :: [Event] -> (Either Error [Primitive], Book)
runE events = runBookState (concat <$> mapM mkState events) emptyBook
