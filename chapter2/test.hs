import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)

g' :: (Int, Int) -> HashMap.HashMap (Int, Int) Int -> Int
g' (x, y) table = min y (max x (HashMap.lookupDefault 0 (x, y) table))

f' :: (Int, Int, Int) -> HashMap.HashMap (Int, Int, Int) Int -> Int
f' (x, y, z) table =
  let t = HashMap.lookupDefault 0 (y, 1, 1) table
  in max (min y z) (HashMap.lookupDefault 0 (z, x, t) table)

fixedpointIteration :: (Eq a, Hashable a) => (a -> HashMap.HashMap a Int -> Int) -> HashMap.HashMap a Int -> (Int, HashMap.HashMap a Int)
fixedpointIteration g table = go table 0
  where
    go prevTable numItr =
      let updatedTable = HashMap.fromList [(k, g k prevTable) | k <- HashMap.keys table]
          isConverge = all (\k -> HashMap.lookupDefault 0 k updatedTable == HashMap.lookupDefault 0 k prevTable) (HashMap.keys table)
      in if isConverge
           then (numItr + 1, updatedTable)
           else go updatedTable (numItr + 1)

main :: IO ()
main = do
  let tableG = HashMap.fromList [((0, 0), 0), ((1, 0), 0), ((0, 1), 0), ((1, 1), 0)]
      resultG = fixedpointIteration g' tableG
  putStrLn $ "Iterations for g: " ++ show (fst resultG)
  print (snd resultG)

  let tableF =
        HashMap.fromList
          [ ((0, 0, 0), 0),
            ((0, 0, 1), 0),
            ((0, 1, 0), 0),
            ((0, 1, 1), 0),
            ((1, 0, 0), 0),
            ((1, 0, 1), 0),
            ((1, 1, 0), 0),
            ((1, 1, 1), 0)
          ]
      resultF = fixedpointIteration f' tableF
  putStrLn $ "Iterations for f: " ++ show (fst resultF)
  print (snd resultF)

