import           AbstLang
import           BaseLang
import           FixedPointIteration

import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable       (Hashable)

-- Define the function definitions
funDefs :: [FunDef]
funDefs =
  [ FunDef "add" ["x", "y"] (Add (Var "x") (Var "y")),
    FunDef "subtract" ["x", "y"] (Sub (Var "x") (Var "y")),
    FunDef "multiply" ["x", "y"] (Mul (Var "x") (Var "y")),
    FunDef "fac" ["n"] (If (Eq (Var "n") (Const 0))
                             (Const 1)
                             (Mul (Var "n") (Call "fac" [(Sub (Var "n") (Const 1))]))),
    FunDef "g" ["x", "y"] (If (Eq (Var "y") (Const 0))
                                (Var "x")
                                (Call "g" [Add (Var "x") (Const 1), Sub (Var "y") (Const 1)]))
  ]

-- Define the expressions

exp1 :: Exp
exp1 = Add (Const 3) (Call "add" [Const 4, Const 5])

exp2 :: Exp
exp2 = Call "fac" [Const 5]

exp3 :: Exp
exp3 = Call "fac" [Var "x"]

exp4 :: Exp
exp4 = Call "g" [Var "x", Var "y"]

-- Evaluate the function definitions

funPhi :: Phi
funPhi = evalProgram funDefs

funAPhi :: APhi
funAPhi = evalAProgram funDefs

-- Evaluate the expressions

result1 :: D
result1 = evalExp exp1 funPhi []

result2 :: D
result2 = evalExp exp2 funPhi []

result3 :: D
result3 = evalExp exp3 funPhi [("x", Just 4)]

result4 :: D
result4 = evalExp exp4 funPhi [("x", Just 4), ("y", Just 3)]

result5 :: ATwo
result5 = evalAExp exp4 funAPhi [("x", One), ("y", Zero)]

result6 :: ATwo
result6 = evalAExp exp4 funAPhi [("x", Zero), ("y", One)]

g' :: [Int] -> HashMap.HashMap [Int] Int -> Int
g' (x:y:rest) table = min y (max x (HashMap.lookupDefault 0 [x, y] table))

f' :: [Int] -> HashMap.HashMap [Int] Int -> Int
f' (x:y:z:rest) table =
  let t = HashMap.lookupDefault 0 [y, 1, 1] table
  in max (min y z) (HashMap.lookupDefault 0 [z, x, t] table)

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

    putStrLn $ "Result1: " ++ show result1
    putStrLn $ "Result2: " ++ show result2
    putStrLn $ "Result3: " ++ show result3
    putStrLn $ "Result4: " ++ show result4
    putStrLn $ "Result5: " ++ show result5
    -- putStrLn $ "Result6: " ++ show result6
