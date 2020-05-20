{-# LANGUAGE ScopedTypeVariables #-}
module Utils.FunctionSpec where
import Test.Hspec
import Test.QuickCheck
import Utils.Function

spec :: Spec
spec = do
    describe "iterFix" $
        it "Repeats division by 2 to get -1 for negative input and 0 otherwise" $
            property $ \(x :: Integer) -> iterFix (`div` 2) x === if x < 0 then -1 else 0 
    describe "composeN" $ do
        it "Composes negation twice to get identity" $
            property $ \(x :: Integer) -> composeN 2 negate x === x
        it "Composes succ small amount of times" $
            property $ \(x :: Integer) (Small (n' :: Int)) ->
                let n = abs n' in composeN n succ x === x + toInteger n
        it "Composes arbitrary function three times" $
            property $ \(Fn f) (x :: Integer) -> composeN 3 f x === f (f (f x))
    