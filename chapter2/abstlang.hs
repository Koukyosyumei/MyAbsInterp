module AbstLang where

import BaseLang

data ATwo = Zero | One deriving(Eq, Show)

alpha :: D -> ATwo
alpha Nothing = Zero
alpha _ = One
            
