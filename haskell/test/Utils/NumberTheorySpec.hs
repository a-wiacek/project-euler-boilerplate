{-# LANGUAGE ScopedTypeVariables #-}
module Utils.NumberTheorySpec where
import Data.Array.Unboxed
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck
import Utils.NumberTheory

arrBound = 100000
primesArr = primesArrayUpTo arrBound
totArr = totientArrayUpTo arrBound

spec :: Spec
spec = do
    describe "primesArrayUpTo" $
        it "Correctly computes primes in range" $ property $ do
            n <- choose (2, arrBound)
            return $ primesArr ! n === isPrime n
    describe "factorize" $ do
        it "Correctly factorizes a number" $ property $ \(Positive (n :: Integer)) ->
            let fact = factorize n
                n1 = Map.foldrWithKey (\p e prod -> unPrime p ^ e * prod) 1 (runFactorization fact)
                n2 = factoredNum fact
            in n === n1 .&&. n === n2
        it "Factorization is a semigroup" $ property $
            \(Positive (b :: Integer)) (Positive (n :: Integer)) (Positive (m :: Integer)) ->
                let fb = factorize b
                    fn = factorize n
                    fm = factorize m
                in (fb <> fn) <> fm === fb <> (fn <> fm)
        it "Factorization is a monoid" $ property $ \(Positive (n :: Integer)) ->
            let fn = factorize n
            in fn <> mempty === fn .&&. mempty <> fn === fn
        it "Factorization correctly factorizes power of a number" $ property $
            \(Positive (n :: Integer)) (Positive (e :: Word)) ->
                factorize (n ^ e) === factPower (factorize n) e
    describe "divisors" $
        it "Computes proper divisors" $ property $ \(Positive (n :: Integer)) ->
            let ds = divisors n
            in all (\d -> n `mod` d == 0) ds
                .&&. numberOfDivisors n === fromIntegral (length ds)
                .&&. sumOfDivisors n === sum ds
    describe "totient" $
        it "Correctly computes totient of a number" $ property $ do
            n <- choose (1, arrBound)
            return $ length (filter (\d -> gcd n d == 1) [1..n]) === totArr ! n
    describe "gcdExt" $
        it "Correctly computes extended Euclidean algorithm" $
            property $ \(a :: Integer) b -> let (x, y, g) = gcdExt a b in a * x + b * y === g
    describe "invertMod" $
        it "Correctly computes modulo inverse (if present)" $
            property $ \(a :: Integer) (Positive (m :: Integer)) -> case invertMod a m of
                Just r -> (r * a) `mod` m === 1 `mod` m
                Nothing -> conjoin $ map (\r -> (r * a) `mod` m =/= 1) [0..m - 1]

