module BaseLang where

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
    | StrictCall String [Exp] deriving(Show)

data FunDef = FunDef String [String] Exp deriving(Show)

evalProgram :: [FunDef] -> Phi
evalProgram funcs = fix (\phi -> applyFunDefs funcs phi)
    where
        fix :: (a -> a) -> a
        fix f = f (fix f)

applyFunDefs :: [FunDef] -> Phi -> Phi
applyFunDefs [] phi = phi
applyFunDefs ((FunDef s args exp):rest) phi =
    applyFunDefs rest ((s, \params -> evalExp exp (updateFunDefs (FunDef s args exp) phi) (zip args params)) : phi)

updateFunDefs :: FunDef -> Phi -> Phi
updateFunDefs (FunDef s args exp) phi =
    let innerphi = [(s, \params -> evalExp exp (updateFunDefs (FunDef s args exp) phi) (zip args params))]
    in innerphi ++ phi

evalExp :: Exp -> Phi -> Env -> D
evalExp (Const x) _ _ = Just x
evalExp (Var key) _ env =
    case lookup key env of
        Nothing     -> Nothing
        Just result -> result
evalExp (Add x y) phi env = (+) <$> (evalExp x phi env) <*> (evalExp y phi env)
evalExp (Sub x y) phi env = (-) <$> (evalExp x phi env) <*> (evalExp y phi env)
evalExp (Mul x y) phi env = (*) <$> (evalExp x phi env) <*> (evalExp y phi env)
evalExp (Eq x y)  phi env = deq (evalExp x phi env) (evalExp y phi env)
    where
        deq :: D -> D -> D
        deq (Just a) (Just b) = if a == b then Just 1 else Just 0
        deq _ _ = Just 0
evalExp (GEq x y) phi env = dgeq (evalExp x phi env) (evalExp y phi env)
    where
        dgeq :: D -> D -> D
        dgeq (Just a) (Just b) = if a >= b then Just 1 else Just 0
        dgeq _ _ = Just 0
evalExp (If cond thenBranch elseBranch) phi env =
    case evalExp cond phi env of
        Nothing -> Nothing
        Just x  -> evalExp (if x == 0 then elseBranch else thenBranch) phi env
evalExp (Call fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Nothing
        Just f  -> f (map (\a -> evalExp a phi env) args)
evalExp (StrictCall fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Nothing
        Just f  -> strict f (map (\a -> evalExp a phi env) args)
    where
        strict :: ([D] -> D) -> [D] -> D
        strict f args =
            if noneIsNothing args then f args else Nothing

noneIsNothing :: [D] -> Bool
noneIsNothing = all isJust
  where
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing  = False
