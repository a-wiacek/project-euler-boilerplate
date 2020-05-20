{-# LANGUAGE ScopedTypeVariables #-}
module Utils.MatrixSpec where
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Utils.Matrix

instance (Integral a, Arbitrary a) => Arbitrary (Matrix a) where
    arbitrary = do
        size <- choose (2, 6)
        fromList size size <$> replicateM (size * size) arbitrary

spec :: Spec
spec = describe "matrixFastPowerMod" $
    it "Raises matrix to the power using fast exponentiation algorithm" $
        property $ \(m :: Matrix Integer) (Positive (p :: Integer)) (NonNegative (e :: Int)) ->
            matrixFastPowerMod m e p === fmap (`mod` p) (foldr multStd (identity $ nrows m) (replicate e m))
