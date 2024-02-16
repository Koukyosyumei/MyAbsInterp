import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable       (Hashable)

g' :: [Int] -> HashMap.HashMap [Int] Int -> Int
g' (x:y:rest) table = min y (max x (HashMap.lookupDefault 0 [x, y] table))

f' :: [Int] -> HashMap.HashMap [Int] Int -> Int
f' (x:y:z:rest) table =
  let t = HashMap.lookupDefault 0 [y, 1, 1] table
  in max (min y z) (HashMap.lookupDefault 0 [z, x, t] table)

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

main :: IO ()
main = do
  let tableG = HashMap.fromList [([0, 0], 0), ([1, 0], 0), ([0, 1], 0), ([1, 1], 0)]
      resultG = fixedpointIteration g' tableG 0
  putStrLn $ "Iterations for g: " ++ show (fst resultG)
  print (snd resultG)

  let tableF =
        HashMap.fromList
          [ ([0, 0, 0], 0),
            ([0, 0, 1], 0),
            ([0, 1, 0], 0),
            ([0, 1, 1], 0),
            ([1, 0, 0], 0),
            ([1, 0, 1], 0),
            ([1, 1, 0], 0),
            ([1, 1, 1], 0)
          ]
      resultF = fixedpointIteration f' tableF 0
  putStrLn $ "Iterations for f: " ++ show (fst resultF)
  print (snd resultF)

