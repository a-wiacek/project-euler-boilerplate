module Utils.Array
    ( modifyArray
    , funArray
    ) where
import Data.Array.IArray
import Data.Array.ST

-- Apply function to element in array.
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr index f = readArray arr index >>= writeArray arr index . f

-- Create array with given bounds and containing values computed using function.
funArray :: (IArray a e, Ix i, Enum i) => i -> i -> (i -> e) -> a i e
funArray minBound maxBound f = array (minBound, maxBound) [(x, f x) | x <- [minBound..maxBound]]