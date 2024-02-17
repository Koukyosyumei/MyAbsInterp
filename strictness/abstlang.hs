module AbstLang where

import           BaseLang
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import           FixedPointIteration

-- | Represents the abstract type 'ATwo' with two values: 'Zero' and 'One'.
data ATwo = Zero | One deriving(Eq, Show)

-- | Instance of 'Hashable' for 'ATwo'.
instance Hashable ATwo where
    hashWithSalt salt Zero = hashWithSalt salt (0 :: Int)
    hashWithSalt salt One  = hashWithSalt salt (1 :: Int)

-- | Maps a 'D' value to an 'ATwo' value.
alpha :: D -> ATwo
alpha Nothing = Zero
alpha _       = One

-- | Logical AND operation for 'ATwo' values.
(∧) :: ATwo -> ATwo -> ATwo
Zero ∧ _ = Zero
_ ∧ Zero = Zero
_ ∧ _    = One

-- | Logical OR operation for 'ATwo' values.
(∨) :: ATwo -> ATwo -> ATwo
One ∨ _ = One
_ ∨ One = One
_ ∨ _   = Zero

-- | Less than or equal comparison for 'ATwo' values.
(<=) :: ATwo -> ATwo -> Bool
Zero <= _ = True
_ <= One  = True
_ <= _    = False

-- | Type alias for memoization table.
type Memo = HashMap.HashMap [ATwo] ATwo

-- | Type alias for environment bindings in the abstract language.
type AEnv = [(String, ATwo)]

-- | Type alias for function signature mappings in the abstract language.
type APhi = [(String, [ATwo] -> Memo -> ATwo)]

-- | The empty memoization table.
emptyMemo :: Memo
emptyMemo = HashMap.empty

-- | Evaluates an abstract program represented by a list of function definitions.
evalAProgram :: [FunDef]  -- ^ List of function definitions.
             -> APhi      -- ^ Function signature mappings.
evalAProgram funcs = fix (\phi -> applyAFunDefs funcs phi)
    where
        fix :: (a -> a) -> a
        fix f = f (fix f)

-- | Applies function definitions to function signature mappings in the abstract language.
applyAFunDefs :: [FunDef]  -- ^ List of function definitions.
              -> APhi      -- ^ Function signature mappings.
              -> APhi      -- ^ Updated function signature mappings.
applyAFunDefs [] phi = phi
applyAFunDefs ((FunDef s args exp):rest) phi =
    applyAFunDefs rest ((s, (\params table -> evalAExpMemo exp table (updateAFunDefs (FunDef s args exp) phi) (zip args params))) : phi)

-- | Updates function signature mappings in the abstract language with a new function definition.
updateAFunDefs :: FunDef  -- ^ Function definition.
               -> APhi    -- ^ Function signature mappings.
               -> APhi    -- ^ Updated function signature mappings.
updateAFunDefs (FunDef s args exp) phi =
    let innerphi = [(s, (\params table -> evalAExpMemo exp table (updateAFunDefs (FunDef s args exp) phi) (zip args params)))]
    in innerphi ++ phi

-- | Evaluates an abstract expression in the abstract language.
evalAExp :: Exp   -- ^ Expression to be evaluated.
         -> APhi  -- ^ Function signature mappings.
         -> AEnv  -- ^ Environment containing variable bindings.
         -> ATwo  -- ^ Result of the evaluation.
evalAExp exp phi env = evalAExpMemo exp emptyMemo phi env

-- | Evaluates an abstract expression with memoization in the abstract language.
evalAExpMemo :: Exp   -- ^ Expression to be evaluated.
             -> Memo  -- ^ Memoization table.
             -> APhi  -- ^ Function signature mappings.
             -> AEnv  -- ^ Environment containing variable bindings.
             -> ATwo  -- ^ Result of the evaluation.
evalAExpMemo (Const _) _ _ _ = One
evalAExpMemo (Var key) _ _ env =
    case lookup key env of
        Nothing     -> Zero
        Just result -> result
evalAExpMemo (If cond thenBranch elseBranch) memo phi env = c ∧ (t ∨ e)
    where
        c = evalAExpMemo cond memo phi env
        t = evalAExpMemo thenBranch memo phi env
        e = evalAExpMemo elseBranch memo phi env
evalAExpMemo (BasicFn fname args) memo phi env =
    case (lookup fname basicPhi) of
        Nothing -> Zero
        Just f  -> foldl (∧) One (map (\a -> evalAExpMemo a memo phi env) args)
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

-- | Transforms an expression to include memoization calls.
transformExpWithMemo :: Exp  -- ^ Expression to transform.
                     -> Exp  -- ^ Transformed expression.
transformExpWithMemo (Const n) = Const n
transformExpWithMemo (Var s) = Var s
transformExpWithMemo (If cond te ee) = If (transformExpWithMemo cond) (transformExpWithMemo te) (transformExpWithMemo ee)
transformExpWithMemo (BasicFn fname args) = BasicFn fname (map (\a -> transformExpWithMemo a) args)
transformExpWithMemo (Call fname args) = MemoCall fname args
