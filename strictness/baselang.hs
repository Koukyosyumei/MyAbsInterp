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
    | If Exp Exp Exp
    | BasicFn String [Exp]
    | Call String [Exp]
    | MemoCall String [Exp]
    | FPICall String [Exp] deriving(Show)

-- | Represents function definitions.
data FunDef = FunDef String [String] Exp deriving(Show)

-- | Signature mappings for basic functions
basicPhi :: Phi
basicPhi = [
            ("add", \(x:y:rest) -> (+) <$> x <*> y),
            ("sub", \(x:y:rest) -> (-) <$> x <*> y),
            ("mul", \(x:y:rest) -> (*) <$> x <*> y),
            ("eq", \((Just x):(Just y):rest) -> if x == y then Just 1 else Just 0),
            ("geq", \((Just x):(Just y):rest) -> if x >= y then Just 1 else Just 0)
           ]

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
evalExp (If cond thenBranch elseBranch) phi env =
    case evalExp cond phi env of
        Nothing -> Nothing
        Just x  -> evalExp (if x == 0 then elseBranch else thenBranch) phi env
evalExp (BasicFn fname args) phi env = 
    case (lookup fname basicPhi) of
        Nothing -> Nothing
        Just f -> strict f (map (\a -> evalExp a phi env) args)
    where
        strict :: ([D] -> D) -> [D] -> D
        strict f args =
            if noneIsNothing args then f args else Nothing
evalExp (Call fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Nothing
        Just f  -> f (map (\a -> evalExp a phi env) args)

-- | Checks if all elements in the list are not 'Nothing'.
noneIsNothing :: [D]  -- ^ List of 'D' values.
              -> Bool -- ^ 'True' if all elements are 'Just', 'False' otherwise.
noneIsNothing = all isJust
  where
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing  = False
