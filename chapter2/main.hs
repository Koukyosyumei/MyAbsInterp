import           BaseLang

fac :: FunDef
fac = FunDef "fac" ["n"] (If (Eq (Var "n") (Const 0))
                             (Const 1)
                             (Mul (Var "n") (Call "fac" [(Sub (Var "n") (Const 1))])))

phi :: Phi
phi = update fac []

main :: IO ()
main = do
    case phi of
        (k, f):_ -> putStrLn $ show (f [Just 4])
    let result = evalExp (Call "fac" [Const 5] ) phi []
    putStrLn $ "Result: " ++ show result
