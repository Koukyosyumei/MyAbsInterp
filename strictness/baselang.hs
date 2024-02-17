module BaseLang where

-- | Type synonym for representing values.
type V = Int

-- | Type synonym for representing computations that may fail.
type D = Maybe Int

-- | Type alias for environment bindings.
type Env = [(String, D)]

-- | Type alias for function signature mappings.
type Phi = [(String, [D] -> D)]

-- | Represents expressions in the language.
data Exp = Const Int
    | Var String
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Eq Exp Exp
    | GEq Exp Exp
    | If Exp Exp Exp
    | Call String [Exp]
    | MemoCall String [Exp]
    | FPICall String [Exp]
    | StrictCall String [Exp] deriving(Show)

-- | Represents function definitions.
data FunDef = FunDef String [String] Exp deriving(Show)

-- | Evaluates a program represented by a list of function definitions.
evalProgram :: [FunDef]    -- ^ List of function definitions.
            -> Phi         -- ^ Function signature mappings.
evalProgram funcs = fix (\phi -> applyFunDefs funcs phi)
    where
        fix :: (a -> a) -> a
        fix f = f (fix f)

-- | Applies function definitions to function signature mappings.
applyFunDefs :: [FunDef]  -- ^ List of function definitions.
             -> Phi       -- ^ Function signature mappings.
             -> Phi       -- ^ Updated function signature mappings.
applyFunDefs [] phi = phi
applyFunDefs ((FunDef s args exp):rest) phi =
    applyFunDefs rest ((s, \params -> evalExp exp (updateFunDefs (FunDef s args exp) phi) (zip args params)) : phi)

-- | Updates function signature mappings with a new function definition.
updateFunDefs :: FunDef  -- ^ Function definition.
              -> Phi     -- ^ Function signature mappings.
              -> Phi     -- ^ Updated function signature mappings.
updateFunDefs (FunDef s args exp) phi =
    let innerphi = [(s, \params -> evalExp exp (updateFunDefs (FunDef s args exp) phi) (zip args params))]
    in innerphi ++ phi

-- | Evaluates an expression in the given environment.
evalExp :: Exp   -- ^ Expression to be evaluated.
        -> Phi   -- ^ Function signature mappings.
        -> Env   -- ^ Environment containing variable bindings.
        -> D     -- ^ Result of the evaluation.
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

-- | Checks if all elements in the list are not 'Nothing'.
noneIsNothing :: [D]  -- ^ List of 'D' values.
              -> Bool -- ^ 'True' if all elements are 'Just', 'False' otherwise.
noneIsNothing = all isJust
  where
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing  = False
