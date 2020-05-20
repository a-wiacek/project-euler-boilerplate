{-# LANGUAGE FlexibleContexts #-}
module Utils.NumberTheory
    ( primesArrayUpTo
    , primesUpTo
    , isPrime
    , Factorization
    , runFactorization
    , factoredNum
    , factorize
    , primePowerFact
    , factPower
    , factMinus
    , divisors
    , sumOfDivisors
    , numberOfDivisors
    , totientArrayUpTo
    , gcdExt
    , invertMod
    -- Reexported from Math.NumberTheory.Primes
    , P.Prime
    , P.primes
    , P.unPrime
    , P.nextPrime
    , P.precPrime
    ) where
import Utils.Array(modifyArray)
import Utils.List(reduceList)
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits(Bits)
import Data.List(group)
import qualified Data.Map.Strict as M
import Data.Maybe(isJust)
import qualified Math.NumberTheory.Primes as P

-- Array with valid indices [1..n]. If 1 <= k <= n, then
-- (primesArrayUpTo n) ! k == True iff k is prime.
primesArrayUpTo :: Int -> UArray Int Bool
primesArrayUpTo n = runSTUArray $ do
    arr <- newArray (1, n) True
    writeArray arr 1 False
    forM_ [2..floor $ sqrt $ fromIntegral n] $ \x -> do
        isPrime <- readArray arr x
        when isPrime $ forM_ [x * x, x * x + x..n] $ \t -> writeArray arr t False
    return arr

-- Produce list of all primes up to n.
primesUpTo :: (Bits a, Integral a, P.UniqueFactorisation a, Enum (P.Prime a)) => a -> [P.Prime a]
primesUpTo n = [P.nextPrime 2 .. P.precPrime n]

-- Check if number is prime.
isPrime :: P.UniqueFactorisation a => a -> Bool
isPrime = isJust . P.isPrime 

-- Structure holding factorization of a number. Keys in map are primes, and values are powers of primes.
data Factorization a = Factorization
    { runFactorization :: !(M.Map (P.Prime a) Word)
    , factoredNum :: !a
    } deriving (Eq, Show)

instance (Num a, Ord a) => Semigroup (Factorization a) where
    Factorization f1 n1 <> Factorization f2 n2 = Factorization (M.unionWith (+) f1 f2) (n1 * n2)

instance (Num a, Ord a) => Monoid (Factorization a) where
    mempty = Factorization M.empty 1

-- Factorize a number. If number is negative, sign factor is not included.
factorize :: (P.UniqueFactorisation a, Ord a) => a -> Factorization a
factorize n = Factorization (M.fromList $ P.factorise n) n

-- Create factorization of power of a prime.
primePowerFact :: Num a => P.Prime a -> Word -> Factorization a
primePowerFact p e = Factorization (M.singleton p e) (P.unPrime p ^ e)

-- Power factorization.
factPower :: Num a => Factorization a -> Word -> Factorization a
factPower (Factorization f n) e = Factorization (M.map (*e) f) (n^e)

-- Take difference of two factorizations (divide factorized numbers).
-- This is not a safe operation: it is not checked whether it is possible to take that difference.
factMinus :: (Ord a, Integral a) => Factorization a -> Factorization a -> Factorization a
factMinus (Factorization f1 n1) (Factorization f2 n2) = Factorization (M.filter (>0) $ M.unionWith (-) f1 f2)
                                                                      (if n2 == 0 then 0 else n1 `div` n2)

-- Get all divisors of number n. If n is negative, both positive and negative divisors are listed.
-- If n == 0, all numbers (except for 0) are listed.
divisors :: Integral a => a -> [a]
divisors n
    | n == 0 = let f x = x : -x : f (x + 1) in f 1
    | n < 0 = let pd = divisors (-n) in pd ++ map negate pd
    | n > 0 = lsq ++ mid ++ reverse (map (div n) lsq) where
        lsq = filter (\x -> n `mod` x == 0) $ takeWhile (\x -> x * x < n) [1..]
        mid = [sq | let sq = floor $ sqrt $ fromIntegral n, sq * sq == n]

-- Compute number of divisors of a number.
numberOfDivisors :: (P.UniqueFactorisation a, Ord a, Integral a) => a -> Word
numberOfDivisors = product . map succ . M.elems . runFactorization . factorize 

-- Compute sum of divisors of a number.
sumOfDivisors :: (P.UniqueFactorisation a, Ord a, Integral a) => a -> a
sumOfDivisors = product . map (\(p', a) -> let p = P.unPrime p' in (p ^ (a + 1) - 1) `div` (p - 1))
              . M.toList . runFactorization . factorize 

-- Array with valid indices [1..n]. If 1 <= k <= n, then
-- (primesArrayUpTo n) ! k is equal to Euler's phi function for k.
totientArrayUpTo :: Int -> UArray Int Int
totientArrayUpTo n = runSTUArray $ do
    arr <- newListArray (1, n) [1..]
    forM_ [2..n] $ \x -> do
        val <- readArray arr x
        when (val == x) $ do
            writeArray arr x (x - 1)
            forM_ [2 * x, 3 * x..n] $ \y ->
                modifyArray arr y ((*(x - 1)) . (`div` x))
    return arr

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a b
    | b == 0 = (signum a, 0, abs a)
    | otherwise = (t, s - q * t, g)
    where (q, r) = a `quotRem` b
          (s, t, g) = gcdExt b r

-- a^(-1) mod m
invertMod :: Integral a => a -> a -> Maybe a
invertMod a m
    | g == 1 = Just (i `mod` m)
    | otherwise = Nothing
    where (i, _, g) = gcdExt a m