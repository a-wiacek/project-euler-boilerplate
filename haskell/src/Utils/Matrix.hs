module Utils.Matrix
    ( module Data.Matrix
    , mulMod
    , matrixFastPowerMod
    ) where
import Data.Matrix

-- Multiply two matrices and take all elements of product modulo p.
mulMod :: Integral a => Matrix a -> Matrix a -> a -> Matrix a
mulMod a b p = fmap (`mod` p) (a `multStd` b)

-- Fast exponentiation algorithm for matrices (analog of fastPowerMod from Utils.Numeric).
-- It is assumend that matrix b is a square matrix.
matrixFastPowerMod :: (Integral a, Integral b) => Matrix a -> b -> a -> Matrix a
matrixFastPowerMod b e p = go b e (identity $ nrows b) where
    go b e acc
        | e == 0 = fmap (`mod` p) acc
        | otherwise = go (f b) (e `div` 2) (if odd e then f acc else acc)
        where f x = mulMod x b p