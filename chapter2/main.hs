import           BaseLang

-- Define your function definitions
funDefs :: [FunDef]
funDefs =
  [ FunDef "add" ["x", "y"] (Add (Var "x") (Var "y")),
    FunDef "subtract" ["x", "y"] (Sub (Var "x") (Var "y")),
    FunDef "multiply" ["x", "y"] (Mul (Var "x") (Var "y")),
    FunDef "fac" ["n"] (If (Eq (Var "n") (Const 0))
                             (Const 1)
                             (Mul (Var "n") (Call "fac" [(Sub (Var "n") (Const 1))])))
  ]

funPhi :: Phi
funPhi = evalProgram funDefs

exp1 :: Exp
exp1 = Add (Const 3) (Call "add" [Const 4, Const 5])

exp2 :: Exp
exp2 = Call "fac" [Const 5]

exp3 :: Exp
exp3 = Call "fac" [Var "x"]

-- Evaluate the example expression using finalPhi
result1 :: D
result1 = evalExp exp1 funPhi []

result2 :: D
result2 = evalExp exp2 funPhi []

result3 :: D
result3 = evalExp exp3 funPhi [("x", Just 4)]

main :: IO ()
main = do
    putStrLn $ "Result1: " ++ show result1
    putStrLn $ "Result2: " ++ show result2
    putStrLn $ "Result3: " ++ show result3
