{-# LANGUAGE ScopedTypeVariables #-}
module Utils.NumericSpec where
import Data.List(group, sort)
import Test.Hspec
import Test.QuickCheck
import Utils.List
import Utils.Numeric

spec :: Spec
spec = do
    describe "reverseInt" $
        it "reverses an integer" $
            property $ \(NonNegative (n :: Integer)) -> reverseInt n === read (reverse (show n))
    describe "binom" $
        it "computes binomial coefficient" $
            property $ \(n' :: Integer) (k' :: Integer) ->
                let [k, n] = sort $ map abs [n', k']
                in binom n k === factorial n `div` (factorial k * factorial (n - k))
    describe "fastPowerMod" $
        it "Raises number to the power using fast exponentiation algorithm" $
            property $ \(m :: Integer) (Positive (p :: Integer)) (NonNegative (e :: Int)) ->
                fastPowerMod m e p === foldr (\a b -> (a * b) `mod` p) 1 (replicate e m) `mod` p
    describe "digits" $
        it "Counts digits of a number" $
            property $ \(m :: Integer) -> digits m === map (pred . snd . reduceList)
                                                           (group $ sort $ show (abs m) ++ "0123456789")
    describe "binsearch" $
        it "Finds largest number satisfying the property" $
            property $ \(q :: Integer) (w :: Integer) (e :: Integer) ->
                let [low, target, high] = sort [q, w, e]
                    f x = x <= target
                in binsearch f low high === target
    describe "digitsSum" $
        it "Computes sum of digits of the number" $
            property $ \(n :: Integer) -> digitsSum n === sum (map (\x -> read [x]) $ show $ abs n)