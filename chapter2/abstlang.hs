module AbstLang where

import           BaseLang
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import           FixedPointIteration


data ATwo = Zero | One deriving(Eq, Show)
instance Hashable ATwo where
    hashWithSalt salt Zero = hashWithSalt salt (0 :: Int)
    hashWithSalt salt One  = hashWithSalt salt (1 :: Int)


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

type Memo = HashMap.HashMap [ATwo] ATwo
type AEnv = [(String, ATwo)]
type APhi = [(String, ([ATwo] -> Memo -> ATwo, Memo))]

emptyMemo :: Memo
emptyMemo = HashMap.empty

evalAProgram :: [FunDef] -> APhi
evalAProgram funcs = fix (\phi -> applyAFunDefs funcs phi)
    where
        fix :: (a -> a) -> a
        fix f = f (fix f)

applyAFunDefs :: [FunDef] -> APhi -> APhi
applyAFunDefs [] phi = phi
applyAFunDefs ((FunDef s args exp):rest) phi =
    applyAFunDefs rest ((s, (\params table -> evalAExp exp (updateAFunDefs (FunDef s args exp) phi) (zip args params), emptyMemo)) : phi)

updateAFunDefs :: FunDef -> APhi -> APhi
updateAFunDefs (FunDef s args exp) phi =
    let innerphi = [(s, 
                    (\params table -> evalAExp exp (updateAFunDefs (FunDef s args exp) phi) (zip args params),
                     emptyMemo))]
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
evalAExp (Eq x y)  phi env = (evalAExp x phi env) ∧ (evalAExp y phi env)
evalAExp (GEq x y) phi env = (evalAExp x phi env) ∧ (evalAExp y phi env)
evalAExp (If cond thenBranch elseBranch) phi env = c ∧ (t ∨ e)
    where
        c = evalAExp cond phi env
        t = evalAExp thenBranch phi env
        e = evalAExp elseBranch phi env
evalAExp (Call fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just (f, _)  -> f (map (\a -> evalAExp a phi env) args) empty
    where
        empty :: HashMap.HashMap [ATwo] ATwo
        empty = HashMap.empty
evalAExp (MemoCall fname args) phi env = 
    case (lookup fname phi) of
        Nothing -> Zero
        Just (_, memo) -> HashMap.lookupDefault Zero (map (\a -> evalAExp a phi env) args) memo
evalAExp (FPICall fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just (f, _)  -> evalWithFPI f (map (\a -> evalAExp a phi env) args) [Zero, One] Zero
evalAExp (StrictCall fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just f  -> foldl (∧) One (map (\a -> evalAExp a phi env) args)
