module AbstLang where

import           BaseLang

data ATwo = Zero | One deriving(Eq, Show)

alpha :: D -> ATwo
alpha Nothing = Zero
alpha _       = One

(∧) :: ATwo -> ATwo -> ATwo
Zero ∧ _ = Zero
_ ∧ Zero = Zero
_ ∧ _    = One

(∨) :: ATwo -> ATwo -> ATwo
One ∨ _ = One
_ ∨ One = One
_ ∨ _   = Zero

(<=) :: ATwo -> ATwo -> Bool
Zero <= _ = True
_ <= One  = True
_ <= _    = False

type AEnv = [(String, ATwo)]
type APhi = [(String, [ATwo] -> ATwo)]

evalAProgram :: [FunDef] -> APhi
evalAProgram funcs = fix (\phi -> applyAFunDefs funcs phi)
    where
        fix :: (a -> a) -> a
        fix f = f (fix f)

applyAFunDefs :: [FunDef] -> APhi -> APhi
applyAFunDefs [] phi = phi
applyAFunDefs ((FunDef s args exp):rest) phi =
    applyAFunDefs rest ((s, \params -> evalAExp exp (updateAFunDefs (FunDef s args exp) phi) (zip args params)) : phi)

updateAFunDefs :: FunDef -> APhi -> APhi
updateAFunDefs (FunDef s args exp) phi =
    let innerphi = [(s, \params -> evalAExp exp (updateAFunDefs (FunDef s args exp) phi) (zip args params))]
    in innerphi ++ phi

evalAExp :: Exp -> APhi -> AEnv -> ATwo
evalAExp (Const x) _ _ = One
evalAExp (Var key) _ env =
    case lookup key env of
        Nothing     -> Zero
        Just result -> result
evalAExp (Add x y) phi env = (evalAExp x phi env) ∧ (evalAExp y phi env)
evalAExp (Sub x y) phi env = (evalAExp x phi env) ∧ (evalAExp y phi env)
evalAExp (Mul x y) phi env = (evalAExp x phi env) ∧ (evalAExp y phi env)
evalAExp (Eq x y)  phi env = deq (evalAExp x phi env) (evalAExp y phi env)
    where
        deq :: ATwo -> ATwo -> ATwo
        deq a b = if a == b then One else Zero
evalAExp (GEq x y) phi env = dgeq (evalAExp x phi env) (evalAExp y phi env)
    where
        dgeq :: ATwo -> ATwo -> ATwo
        dgeq One _    = One
        dgeq Zero One = One
        dgeq _ _      = Zero
evalAExp (If cond thenBranch elseBranch) phi env = c ∧ (t ∨ e)
    where
        c = evalAExp cond phi env
        t = evalAExp thenBranch phi env
        e = evalAExp elseBranch phi env
evalAExp (Call fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just f  -> f (map (\a -> evalAExp a phi env) args)
evalAExp (StrictCall fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just f  -> foldl (∧) One (map (\a -> evalAExp a phi env) args)
