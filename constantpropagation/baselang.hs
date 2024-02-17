module BaseLang where

data D = Bottom | Top | B Bool | N Int deriving(Show, Eq)

-- | Type alias for environment bindings.
type Env = [(String, D)]

-- | Type alias for function signature mappings.
type Phi = [(String, [D] -> D)]

-- | Represents expressions in the language.
data Exp = Const D
    | Var String
    | BasicFn String [Exp]
    | Call String [Exp]
    | MemoCall String [Exp]
    | FPICall String [Exp] deriving(Show)

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

addD :: [D] -> D
addD ((N x):(N y):_) = N (x + y)
addD (Bottom:_:_) = Bottom
addD (_:Bottom:_) = Bottom

subD :: [D] -> D
subD ((N x):(N y):_) = N (x - y)
subD (Bottom:_:_) = Bottom
subD (_:Bottom:_) = Bottom

mulD :: [D] -> D 
mulD ((N x):(N y):_) = N (x * y)
mulD (Bottom:_:_) = Bottom
mulD (_:Bottom:_) = Bottom

eqD :: [D] -> D
eqD ((N x):(N y):_) = B (x == y)
eqD ((B x):(B y):_) = B (x == y)
eqD (Bottom:_:_) = Bottom
eqD (_:Bottom:_) = Bottom

-- | Signature mappings for basic functions
basicPhi :: Phi
basicPhi = [("add", addD), ("sub", subD), ("mul", mulD), ("eq", eqD)]

-- | Evaluates an expression in the given environment.
evalExp :: Exp   -- ^ Expression to be evaluated.
        -> Phi   -- ^ Function signature mappings.
        -> Env   -- ^ Environment containing variable bindings.
        -> D     -- ^ Result of the evaluation.
evalExp (Const x) _ _ = x
evalExp (Var key) _ env =
    case lookup key env of
        Nothing     -> Bottom
        Just result -> result
evalExp (BasicFn fname args) phi env = 
    case (lookup fname basicPhi) of
        Nothing -> Bottom
        Just f -> sanitize f (map (\a -> evalExp a phi env) args)
    where
        sanitize :: ([D] -> D) -> [D] -> D
        sanitize f args =
            if noneIsTop args then f args else Top
evalExp (Call fname args) phi env =
    case (lookup fname phi) of
        Nothing -> Bottom
        Just f  -> f (map (\a -> evalExp a phi env) args)

-- | Checks if all elements in the list are not 'Nothing'.
noneIsTop :: [D]  -- ^ List of 'D' values.
              -> Bool -- ^ 'True' if there is at least one 'Top' in the input list.
noneIsTop = any isTop
  where
    isTop :: D -> Bool
    isTop Top = True
    isTop _  = False
