module Bacus.Closing (closingPairs) where

import Bacus.Types
import qualified Data.Map as Map

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
-- May reuse for netting of permanent accounts
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
