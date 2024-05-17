import           Strictness
import           FixedPointIteration

import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable       (Hashable)

-- Define the demo functions for fixed point iteration
g' :: [Int] -> HashMap.HashMap [Int] Int -> Int
g' (x:y:rest) table = min y (max x (HashMap.lookupDefault 0 [x, y] table))

f' :: [Int] -> HashMap.HashMap [Int] Int -> Int
f' (x:y:z:rest) table =
  let t = HashMap.lookupDefault 0 [y, 1, 1] table
  in max (min y z) (HashMap.lookupDefault 0 [z, x, t] table)

-- Define the function expressions
add :: Exp
add = BasicFn "add" [Var "x", Var "y"]

fac :: Exp
fac = If (BasicFn "eq" [Var "n", Const 0])
         (Const 1)
         (BasicFn "mul" [Var "n", Call "fac" [BasicFn "sub" [Var "n", Const 1]]])
g :: Exp
g = If (BasicFn "eq" [Var "y", Const 0])
       (Var "x")
       (Call "g" [BasicFn "add" [Var "x", Const 1], BasicFn "sub" [Var "y", Const 1]])

-- Define the function definitions
funDefs :: [FunDef]
funDefs =
  [ FunDef "add" ["x", "y"] add,
    FunDef "fac" ["n"] fac,
    FunDef "g" ["x", "y"] g,
    FunDef "g'" ["x", "y"] (transformExpWithMemo g)
  ]

-- Evaluate the function definitions

funPhi :: Phi
funPhi = evalProgram funDefs

funAPhi :: APhi
funAPhi = evalAProgram funDefs

-- Define the expressions

callAdd :: Exp
callAdd = BasicFn "add" [Const 3, Call "add" [Const 4, Const 5]]

callFacConst :: Exp
callFacConst = Call "fac" [Const 5]

callFacVar :: Exp
callFacVar = Call "fac" [Var "x"]

callG :: Exp
callG = Call "g" [Var "x", Var "y"]

fpicallG' :: Exp
fpicallG' = FPICall "g'" [Var "x", Var "y"]

-- Evaluate the expressions

main :: IO ()
main = do
    let tableG = HashMap.fromList [([0, 0], 0), ([1, 0], 0), ([0, 1], 0), ([1, 1], 0)]
        resultG = fixedpointIteration g' tableG 0
    putStrLn $ "Fixed point iterations for g: " ++ show (fst resultG)
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
    putStrLn $ "Fixed point iterations for f: " ++ show (fst resultF)
    print (snd resultF)
    putStrLn "-----------------------"

    let resultCallAdd = evalExp callAdd funPhi []
        resultCallFacConst = evalExp callFacConst funPhi []
        resultCallFacVar = evalExp callFacVar funPhi [("x", Just 4)]
        resultCallG = evalExp callG funPhi [("x", Just 4), ("y", Just 3)]
        resultFPICallGZeroZero = evalAExp fpicallG' funAPhi [("x", Zero), ("y", Zero)]
        resultFPICallGOneZero = evalAExp fpicallG' funAPhi [("x", One), ("y", Zero)]
        resultFPICallGZeroOne = evalAExp fpicallG' funAPhi [("x", Zero), ("y", One)]
        resultFPICallGOneOne = evalAExp fpicallG' funAPhi [("x", One), ("y", One)]
    putStrLn $ "add(4, 5) = " ++ show (resultCallAdd)
    putStrLn $ "fac(5) = " ++ show (resultCallFacConst)
    putStrLn $ "x = 4; fac(x) = " ++ show (resultCallFacVar)
    putStrLn $ "x = 4; y = 3; g(4, 3) = " ++ show (resultCallG)
    putStrLn $ "x = Zero; y = Zero; g#(x, y) = " ++ show (resultFPICallGZeroZero)
    putStrLn $ "x = One; y = Zero; g#(x, y) = " ++ show (resultFPICallGOneZero)
    putStrLn $ "x = Zero; y = One; g#(x, y) = " ++ show (resultFPICallGZeroOne)
    putStrLn $ "x = One; y = One; g#(x, y) = " ++ show (resultFPICallGOneOne)
