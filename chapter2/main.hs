import           BaseLang

fac :: FunDef
fac = FunDef "fac" ["n"] (If (Eq (Var "n") (Const 0))
                             (Const 1)
                             (Mul (Var "n") (Call "fac" [(Sub (Var "n") (Const 1))])))

phi :: Phi
phi = updateFunDefs fac []

-- Define your function definitions
funcDefs :: [FunDef]
funcDefs =
  [ FunDef "add" ["x", "y"] (Add (Var "x") (Var "y")),
    FunDef "subtract" ["x", "y"] (Sub (Var "x") (Var "y")),
    FunDef "multiply" ["x", "y"] (Mul (Var "x") (Var "y"))
  ]

-- Use progSemantics to get the final function environment
finalPhi :: Phi
finalPhi = evalProgram funcDefs

exampleExp :: Exp
exampleExp = Add (Const 3) (Call "add" [Const 4, Const 5])

-- Evaluate the example expression using finalPhi
result :: D
result = evalExp exampleExp finalPhi []

main :: IO ()
main = do
    putStrLn $ show result
    putStrLn $ show fac
    case phi of
        (k, f):_ -> putStrLn $ show (f [Just 4])
    let result = evalExp (Call "fac" [Const 5] ) phi []
    putStrLn $ "Result: " ++ show result
