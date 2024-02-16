import           BaseLang
import           AbstLang

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

main :: IO ()
main = do
    putStrLn $ "Result1: " ++ show result1
    putStrLn $ "Result2: " ++ show result2
    putStrLn $ "Result3: " ++ show result3
    putStrLn $ "Result4: " ++ show result4
    putStrLn $ "Result5: " ++ show result5
    putStrLn $ "Result6: " ++ show result6
