module Utils.Function
    ( iterFix
    , composeN
    ) where

-- Given function and initial input, iterate function until a fixpoint is found.
iterFix :: Eq a => (a -> a) -> a -> a
iterFix f x = let y = f x in if x == y then x else iterFix f y

-- Compose function with itself n times.
composeN :: Int -> (a -> a) -> a -> a
composeN n f = foldr (.) id (replicate n f)