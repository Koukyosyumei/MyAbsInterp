module FixedPointIteration where

import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable       (Hashable)

-- | Performs fixed-point iteration on a function until convergence.
--
-- Given a function @g@, an initial table, and a default value, this function
-- iteratively applies @g@ to each key in the table until convergence is reached,
-- i.e., until no further changes occur in the table.
--
-- Returns a tuple containing the number of iterations performed and the final table.
fixedpointIteration :: (Eq a, Hashable a, Eq b) =>
                        (a -> HashMap.HashMap a b -> b)  -- ^ The function to apply.
                     -> HashMap.HashMap a b            -- ^ The initial table.
                     -> b                              -- ^ The default value.
                     -> (Int, HashMap.HashMap a b)     -- ^ (Number of iterations, Final table)
fixedpointIteration g table defaultval = go table 0
  where
    go prevTable numItr =
      let updatedTable = HashMap.fromList [(k, g k prevTable) | k <- HashMap.keys table]
          isConverge = all (\k -> HashMap.lookupDefault defaultval k updatedTable == HashMap.lookupDefault defaultval k prevTable)
                           (HashMap.keys table)
      in if isConverge
           then (numItr + 1, updatedTable)
           else go updatedTable (numItr + 1)

-- | Evaluates a function using fixed-point iteration.
--
-- Given a function @f@, a list of arguments, a domain for the arguments,
-- and a default value, this function performs fixed-point iteration to find
-- the value of @f@ for the given arguments.
evalWithFPI :: (Eq a, Hashable a, Eq b) =>
                ([a] -> HashMap.HashMap [a] b -> b)  -- ^ The function to evaluate.
                -> [a]                           -- ^ List of arguments.
                -> [a]                         -- ^ Domain for the arguments.
                -> b                           -- ^ Default value.
                -> b                           -- ^ Result of evaluation.
evalWithFPI f args domain defaultval = 
    HashMap.lookupDefault defaultval args (snd (fixedpointIteration f table defaultval))
    where
        table = HashMap.fromList [(k, defaultval) | k <- (allCombinations (length args) domain)]
 
-- | Generates all combinations of elements from a list.
allCombinations :: Int    -- ^ Number of elements in each combination.
                -> [a]    -- ^ The list of elements.
                -> [[a]]  -- ^ List of all combinations.
allCombinations n xs = allCombinations' (replicate n xs)

-- | Helper function for generating all combinations.
allCombinations' :: [[a]] -> [[a]]
allCombinations' [] = [[]]
allCombinations' (xs:xss) = [x:ys | x <- xs, ys <- allCombinations' xss]   
