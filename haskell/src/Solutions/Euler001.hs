module Solutions.Euler001 where

-- k + 2k + ... + k (bound `div` k) = k (1 + ... + bound `div` k)
sumDivisibleUpTo :: Int -> Int -> Int
sumDivisibleUpTo k bound = let l = bound `div` k in k * l * (l + 1) `div` 2

euler001 :: IO String
euler001 = return $ show $ sumDivisibleUpTo 3 999 + sumDivisibleUpTo 5 999 - sumDivisibleUpTo 15 999