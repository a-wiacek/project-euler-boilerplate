module Utils.Operators
    ( (<:>)
    , (<&&>)
    , (<||>)
    ) where

-- Apply two functions to argument and create pair consisting of both outputs.
infixr 3 <:>
(<:>) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f <:> g) v = (f v, g v)

-- Combine two predicates into one to check if element satisfies both of them.
infixr 3 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(f <&&> g) v = f v && g v

-- Combine two predicates into one to check if element satisfies any of them.
infixr 2 <||>
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(f <||> g) v = f v || g v