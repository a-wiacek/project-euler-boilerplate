module Utils.Numeric
    ( fibonacci
    , reverseInt
    , isPalindrome
    , isInt
    , isSquare
    , factorial
    , binom
    , multinom
    , fastPowerMod
    , digitsInBase
    , digits
    , pandigitalDigitsK
    , pandigitalDigits
    , isPandigitalK
    , isPandigital
    , binsearch
    , bisection
    , digitsSum
    , digitalRoot
    ) where
import Control.Monad(forM_)
import Control.Monad.ST
import Data.Array.ST
import Utils.Array(modifyArray)
import Utils.Function(iterFix)

-- Infinite family of lists of Fibonacci numbers with chosen base values.
fibonacci :: Integral a => a -> a -> [a]
fibonacci a b = a : fibonacci b (a + b)

-- Compute reverse of a nonnegative number.
reverseInt :: Integral a => a -> a
reverseInt n = reverseInt' n 0 where
    reverseInt' 0 y = y
    reverseInt' x y = reverseInt' (x `div` 10) (10 * y + x `mod` 10)

-- Check if a nonnegative number is a palindrome.
isPalindrome :: Integral a => a -> Bool
isPalindrome n = n == reverseInt n

-- Check if a number is an integer.
isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

-- Check if a number is a square.
isSquare :: Integral a => a -> Bool
isSquare x = x == d * d where d = floor $ sqrt $ fromIntegral x

-- Factorial
factorial :: Integral a => a -> a
factorial n = product [1..n]

-- Binomial coefficient
binom :: Integral a => a -> a -> a
binom x y
    | y > x || y < 0 = 0
    | otherwise = iter 1 1
    where y' = if y + y < x then y else x - y
          iter s acc | s > y' = acc
                     | otherwise = iter (s + 1) (acc * (x + 1 - s) `div` s)

-- Multinomial coefficient
-- It is assumed that all elements in list are nonnegative.
multinom :: Integral a => [a] -> a
multinom l = product [1..sum l] `div` product (map factorial l)

-- b^e mod m using fast exponentiation algorithm
fastPowerMod :: (Integral a, Integral b) => a -> b -> a -> a
fastPowerMod b e m = go b e 1 where
    go b e acc
        | e == 0 = acc `mod` m
        | otherwise = go (f b) (e `div` 2) (if odd e then f acc else acc)
        where f x = x * b `mod` m

-- Count digits of number. kth element of output list gives information about number of digits k in n.
-- 0 is assumed to have one digit 0. Base is assumed to be >= 2.
digitsInBase :: Integral a => a -> Int -> [Int]
digitsInBase n b
    | n == 0 = 1 : replicate (b - 1) 0
    | otherwise = runST $ do
        arr <- newArray (0, b - 1) 0 :: ST s (STUArray s Int Int)
        let b' = fromIntegral b
        forM_ (takeWhile (/= 0) $ iterate (`quot` b') n) $ \x ->
            modifyArray arr (fromIntegral $ abs (x `rem` b')) succ
        getElems arr

digits :: Integral a => a -> [Int]
digits n = digitsInBase n 10

-- Number is k-pandigital, if it contains all digits from 1 to k exactly one.
-- Number is pandigitial if it is 9-pandigital.

pandigitalDigitsK :: Int -> [Int]
pandigitalDigitsK k = 0 : (replicate k 1 ++ replicate (9 - k) 0)

pandigitalDigits :: [Int]
pandigitalDigits = pandigitalDigitsK 9

isPandigitalK :: Integral a => a -> Int -> Bool
isPandigitalK n k = digits n == pandigitalDigitsK k

isPandigital :: Integral a => a -> Bool
isPandigital n = isPandigitalK n 9

-- Find largest number in interval satisfying function.
binsearch :: Integral a => (a -> Bool) -> a -> a -> a
binsearch f = bf where
    bf low high | low >= high = low
                | f mid = bf mid high
                | otherwise = bf low (mid - 1)
                where mid = (low + high + 1) `div` 2

-- Perform iterations of bisection method. It is assumed that l < h
-- and signum l /= signum h.
bisection :: (Fractional a, Eq b, Num b) => (a -> b) -> Int -> a -> a -> a
bisection f iters l h = bs iters l (f l) h (f h) where
    bs 0 low _ high _ = (low + high) / 2
    bs its low flow high fhigh =
        let mid = (low + high) / 2
            fmid = f mid
        in if signum fmid == signum flow
            then bs (its - 1) mid fmid high fhigh
            else bs (its - 1) low flow mid fmid

-- Compute sum of digits of number.
digitsSum :: Integral a => a -> a
digitsSum n = go 0 (abs n) where
    go acc n | n == 0 = acc
             | otherwise = go (acc + n `mod` 10) (n `div` 10)

-- Compute digital root of number
digitalRoot :: Integral a => a -> a
digitalRoot = iterFix digitsSum