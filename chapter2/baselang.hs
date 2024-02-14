import Debug.Trace

type V = Int
type D = Maybe Int

type Env = [(String, D)]
type Phi = [(String, [D] -> D)]

data Exp = Const Int
    | Var String
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Eq Exp Exp
    | GEq Exp Exp
    | If Exp Exp Exp
    | Call String [Exp]

data FunDef = FunDef String [String] Exp
data Program = FunDefs [FunDef]

-- evalProgram :: Program -> Env -> Phi
-- update :: String -> 
update :: FunDef -> Phi -> Phi
update (FunDef s args exp) phi = 
    let innerphi = [(s, \params -> evalExp exp (update (FunDef s args exp) phi) (zip args params))]
    in innerphi ++ phi

evalExp :: Exp -> Phi -> Env -> D
evalExp (Const x) _ _ = Just x
evalExp (Var key) _ env = 
    case lookup key env of 
        Nothing -> Nothing
        Just result -> result
evalExp (Add x y) phi env = (+) <$> (evalExp x phi env) <*> (evalExp y phi env)
evalExp (Sub x y) phi env = (-) <$> (evalExp x phi env) <*> (evalExp y phi env)
evalExp (Mul x y) phi env = (*) <$> (evalExp x phi env) <*> (evalExp y phi env)
evalExp (Eq x y)  phi env = deq (evalExp x phi env) (evalExp y phi env)
    where
        deq :: D -> D -> D
        deq (Just a) (Just b) =
            if a == b
                then Just 1
                else Just 0
evalExp (GEq x y) phi env = dgeq (evalExp x phi env) (evalExp y phi env)
    where
        dgeq :: D -> D -> D
        dgeq (Just a) (Just b) =
            if a >= b
                then Just 1
                else Just 0
evalExp (If cond thenBranch elseBranch) phi env =
    case evalExp cond phi env of
        Nothing -> Nothing
        Just x  -> evalExp (if x == 0 then elseBranch else thenBranch) phi env
evalExp (Call fname args) phi env = 
    case (lookup fname phi) of 
        Nothing -> Nothing
        Just f -> f (map (\a -> evalExp a phi env) args)

fac :: FunDef
fac = FunDef "fac" ["n"] (If (Eq (Var "n") (Const 0)) (Const 1) (Mul (Var "n") (Call "fac" [(Sub (Var "n") (Const 1))])))

phi :: Phi
phi = update fac []

main :: IO ()
main = do
    case phi of 
        (k, f):_ -> putStrLn $ show (f [Just 4])
    let result = evalExp (Call "fac" [Const 5] ) phi []
    putStrLn $ "Result: " ++ show result
