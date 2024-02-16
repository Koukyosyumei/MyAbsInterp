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
type APhi = [(String, [ATwo] -> Memo -> ATwo)]

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
    applyAFunDefs rest ((s, (\params table -> evalAExpMemo exp table (updateAFunDefs (FunDef s args exp) phi) (zip args params))) : phi)

updateAFunDefs :: FunDef -> APhi -> APhi
updateAFunDefs (FunDef s args exp) phi =
    let innerphi = [(s, (\params table -> evalAExpMemo exp table (updateAFunDefs (FunDef s args exp) phi) (zip args params)))]
    in innerphi ++ phi

evalAExp :: Exp -> APhi -> AEnv -> ATwo
evalAExp exp phi env = evalAExpMemo exp emptyMemo phi env

evalAExpMemo :: Exp -> Memo -> APhi -> AEnv -> ATwo
evalAExpMemo (Const _) _ _ _ = One
evalAExpMemo (Var key) _ _ env =
    case lookup key env of
        Nothing     -> Zero
        Just result -> result
evalAExpMemo (Add x y) memo phi env = (evalAExpMemo x memo phi env) ∧ (evalAExpMemo y memo phi env)
evalAExpMemo (Sub x y) memo phi env = (evalAExpMemo x memo phi env) ∧ (evalAExpMemo y memo phi env)
evalAExpMemo (Mul x y) memo phi env = (evalAExpMemo x memo phi env) ∧ (evalAExpMemo y memo phi env)
evalAExpMemo (Eq x y)  memo phi env = (evalAExpMemo x memo phi env) ∧ (evalAExpMemo y memo phi env)
evalAExpMemo (GEq x y) memo phi env = (evalAExpMemo x memo phi env) ∧ (evalAExpMemo y memo phi env)
evalAExpMemo (If cond thenBranch elseBranch) memo phi env = c ∧ (t ∨ e)
    where
        c = evalAExpMemo cond memo phi env
        t = evalAExpMemo thenBranch memo phi env
        e = evalAExpMemo elseBranch memo phi env
evalAExpMemo (Call fname args) memo phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just f  -> f (map (\a -> evalAExpMemo a memo phi env) args) emptyMemo
evalAExpMemo (MemoCall fname args) memo phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just _ -> HashMap.lookupDefault Zero (map (\a -> evalAExpMemo a memo phi env) args) memo
evalAExpMemo (FPICall fname args) memo phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just f  -> evalWithFPI f (map (\a -> evalAExpMemo a memo phi env) args) [Zero, One] Zero
evalAExpMemo (StrictCall fname args) memo phi env =
    case (lookup fname phi) of
        Nothing -> Zero
        Just f  -> foldl (∧) One (map (\a -> evalAExpMemo a memo phi env) args)
