module FixedPointIteration where

import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable       (Hashable)

fixedpointIteration :: (Eq a, Hashable a, Eq b) =>
                        (a -> HashMap.HashMap a b -> b) -> HashMap.HashMap a b -> b
                        -> (Int, HashMap.HashMap a b)
fixedpointIteration g table defaultval = go table 0
  where
    go prevTable numItr =
      let updatedTable = HashMap.fromList [(k, g k prevTable) | k <- HashMap.keys table]
          isConverge = all (\k -> HashMap.lookupDefault defaultval k updatedTable == HashMap.lookupDefault defaultval k prevTable)
                           (HashMap.keys table)
      in if isConverge
           then (numItr + 1, updatedTable)
           else go updatedTable (numItr + 1)


evalWithFPI :: (Eq a, Hashable a, Eq b) => ([a] -> HashMap.HashMap [a] b -> b) -> [a] -> [a] -> b -> b
evalWithFPI f args domain defaultval = 
    HashMap.lookupDefault defaultval args (snd (fixedpointIteration f table defaultval))
    where
        table = HashMap.fromList [(k, defaultval) | k <- (allCombinations (length args) domain)]
       
allCombinations :: Int -> [a] -> [[a]]
allCombinations n xs = allCombinations' (replicate n xs)

allCombinations' :: [[a]] -> [[a]]
allCombinations' [] = [[]]
allCombinations' (xs:xss) = [x:ys | x <- xs, ys <- allCombinations' xss]        
