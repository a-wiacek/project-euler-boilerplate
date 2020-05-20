{-# LANGUAGE ScopedTypeVariables #-}
module Utils.QueueSpec where
import Test.Hspec
import Test.QuickCheck
import Utils.Queue

instance Arbitrary a => Arbitrary (Queue a) where
    arbitrary = do
        q <- reorder . fromList <$> arbitrary
        foldr enqueue q <$> (arbitrary :: Gen [a])

spec :: Spec
spec = describe "Queue" $ do
    it "Is a semigroup" $ property $
        \(q :: Queue Int) (w :: Queue Int) (e :: Queue Int) -> (q <> w) <> e == q <> (w <> e)
    it "Is a monoid" $ property $ \(q :: Queue Int) -> q <> mempty == q && mempty <> q == q
    it "Reorder preserves order of elemets" $ property $ \(q :: Queue Int) -> q == reorder q
    it "Peek returns last element" $ property $ \(q :: Queue Int) -> size q > 0 ==>
        let (ePeek, qPeek) = peek q
            (eDequeue, qDequeue) = dequeue q
        in ePeek === eDequeue .&&. init (toList qPeek) === toList qDequeue