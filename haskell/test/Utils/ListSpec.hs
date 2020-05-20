{-# LANGUAGE ScopedTypeVariables #-}
module Utils.ListSpec where
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Ord
import Utils.List
import Utils.Numeric(binom)

noDuplicates :: Eq a => [a] -> Bool
noDuplicates (x:y:z) = x /= y && noDuplicates (y:z)
noDuplicates _ = True

combinationPairs :: [(Int, Int)]
combinationPairs = [(x, y) | x <- [1..15], y <- [0..x]]

data ListWithIndex a = ListWithIndex [a] Int deriving Show
instance Arbitrary a => Arbitrary (ListWithIndex a) where
    arbitrary = do
        NonEmpty l <- arbitrary
        n <- choose (0, length l - 1)
        return $ ListWithIndex l n

uniqueRemainders :: Int -> [Int] -> Bool
uniqueRemainders n l = let l' = map (`mod` n) l in length (uniques l) == length l

listsEqualExceptForIndex :: Eq a => Int -> [a] -> [a] -> Bool
listsEqualExceptForIndex _ [] [] = True
listsEqualExceptForIndex _ [] _ = False
listsEqualExceptForIndex _ _ [] = False
listsEqualExceptForIndex 0 (_:a) (_:b) = listsEqualExceptForIndex (-1) a b
listsEqualExceptForIndex n (a:b) (x:y) = a == x && listsEqualExceptForIndex (n - 1) b y

consecutivesSatisfy :: (a -> a -> Bool) -> [a] -> Bool
consecutivesSatisfy f = go where
    go (a:b:t) = f a b && go (b:t)
    go _ = True

spec :: Spec
spec = do
    describe "reduceList" $
        it "Reduces properly list" $
            property $ \(Small (n' :: Int)) (c :: Int) ->
                let n = abs n' + 1 in reduceList (replicate n c) === (c, n)
    describe "combinations" $ do -- only small tests because of exponential time complexity
        it "Returns as much combinations as expected" $
            conjoin $ map (\(x, y) -> length (uniques $ combinations y [1..x]) === binom x y) combinationPairs
        it "Each combination has expected amount of elements" $
            conjoin $ map (\(x, y) -> conjoin $ map (\l -> length l === y) $ combinations y [1..x]) combinationPairs
    describe "combinations2" $ do -- only small tests because of exponential time complexity
        it "Returns as much combinations as expected" $
            conjoin $ map (\(x, y) -> length (uniques $ combinations2 y [1..x]) === binom x y) combinationPairs
        it "Each combination has expected amount of elements" $
            conjoin $ map
                (\(x, y) -> conjoin $ map
                    (\(s, r) -> length s === y .&&. length r === x - y)
                    (combinations2 y [1..x]))
                combinationPairs
    describe "powerset" $ do -- only small tests because of exponential time complexity
        it "Returns as much elements as expected" $
            conjoin $ map (\x -> length (uniques $ powerset [1..x]) === 2^x) [0..15]
        it "Returns empty list only once" $
            conjoin $ map (\x -> length (filter null $ powerset [1..x]) === 1) [0..15]
    describe "nubSorted" $
        it "Does not leave duplicates next to each other" $
            property $ \(s :: [Int]) -> noDuplicates (nubSorted s)
    describe "splitOn" $ do
        it "Does not leave separator in output lists" $
            property $ \(c :: Int) (s :: [Int]) -> expectFailure (disjoin $ map (elem c) (splitOn c s))
        it "Returns to previous value after joining output lists using separator" $
            property $ \(c :: Int) (s :: [Int]) -> intercalate [c] (splitOn c s) === s
    describe "allEqual" $ do
        it "Confirms that all elements of list are equal if they indeed are" $
            property $ \(m :: Int) (Small (n :: Int)) -> allEqual (replicate (abs n) m)
        it "Returns false if some elements of list differ" $
            property $ \(l :: [Int]) -> expectFailure (allEqual $ l ++ [length l, length l + 1])
    describe "uniquesBy" $
        it "Leaves at most one element from each class of congruence" $
            property $ \(l :: [Int]) -> uniqueRemainders 10 (uniquesBy (\x y -> (x - y) `mod` 10 == 0) l)
    describe "updateAt" $ do
        it "Does not do anything if first argument is greater than length of list" $
            property $ \(l :: [Int]) -> l === updateAt (length l) succ l
        it "Updates properly element of non-empty list" $
            property $ \(ListWithIndex (l :: [Int]) n) (Fn f) ->
                let l' = updateAt n f l
                in f (l !! n) === l' !! n .&&. listsEqualExceptForIndex n l l' 
    describe "deleteAt" $ do
        it "Does not do anything if first argument is greater than length of list" $
            property $ \(l :: [Int]) -> l === deleteAt (length l) l
        it "Removes properly element of nonempty list" $
            property $ \(ListWithIndex (l :: [Int]) n) ->
                let l' = deleteAt n l
                in take n l' === take n l .&&. drop n l' === drop (n + 1) l
    describe "takeLast" $ do
        it "Does not find anything for falsey predicate" $
            property $ \(l :: [Int]) -> isNothing (takeLast (const False) l)
        it "Finds last element for non-empty list" $
            property $ \(NonEmpty (l :: [Int])) -> takeLast (const True) l === Just (last l)
    describe "ascendingSum" $ do
        it "Computes union of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in sort (l1 `union` l2) === ascendingSum l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in consecutivesSatisfy (<) (ascendingSum l1 l2)
        it "Is symmetric" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in ascendingSum l1 l2 === ascendingSum l2 l1
    describe "descendingSum" $ do
        it "Computes union of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in sortOn Down (l1 `union` l2) === descendingSum l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in consecutivesSatisfy (>) (descendingSum l1 l2)
        it "Is symmetric" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in descendingSum l1 l2 === descendingSum l2 l1
    describe "ascendingIntersection" $ do
        it "Computes intersection of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in sort (l1 `intersect` l2) === ascendingIntersection l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in consecutivesSatisfy (<) (ascendingIntersection l1 l2)
        it "Is symmetric" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in ascendingIntersection l1 l2 === ascendingIntersection l2 l1
    describe "descendingIntersection" $ do
        it "Computes intersection of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in sortOn Down (l1 `intersect` l2) === descendingIntersection l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in consecutivesSatisfy (>) (descendingIntersection l1 l2)
        it "Is symmetric" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in descendingIntersection l1 l2 === descendingIntersection l2 l1
    describe "ascendingMinus" $ do
        it "Computes difference of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in sort (l1 \\ l2) === ascendingMinus l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in consecutivesSatisfy (<) (ascendingMinus l1 l2)
    describe "descendingMinus" $ do
        it "Computes difference of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in sortOn Down (l1 \\ l2) === descendingMinus l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in consecutivesSatisfy (>) (descendingMinus l1 l2)
    describe "ascendingXor" $ do
        it "Computes xor of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in sort ((l1 \\ l2) `union` (l2 \\ l1)) === ascendingXor l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = nubSorted l1'
                    l2 = nubSorted l2'
                in consecutivesSatisfy (<) (ascendingXor l1 l2)
    describe "descendingXor" $ do
        it "Computes xor of two ordered lists" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in sortOn Down ((l1 \\ l2) `union` (l2 \\ l1)) === descendingXor l1 l2
        it "Preserves order of list without having duplicates" $
            property $ \(Ordered (l1' :: [Int])) (Ordered (l2' :: [Int])) ->
                let l1 = reverse $ nubSorted l1'
                    l2 = reverse $ nubSorted l2'
                in consecutivesSatisfy (>) (descendingXor l1 l2)
    describe "foldDescend" $ do
        it "Takes all but the last one elements of nonempty list if supplied with const" $
            property $ \(NonEmpty (l :: [Int])) -> foldDescend const l === init l
        it "Takes tail of nonempty list if supplied with function returning second argument" $
            property $ \(NonEmpty (l :: [Int])) -> foldDescend (const id) l === tail l
    describe "everyOther, interweave" $ do
        it "Halves length of list" $
            property $ \(l :: [Int]) -> length l `div` 2 === length (everyOther False l)
        it "Splits list into two equally sized parts" $
            property $ \(l :: [Int]) -> interweave (everyOther True l) (everyOther False l) === l
    describe "picks" $
        it "Generates pairs of element from list and remaining ones" $
            property $ \(Ordered (l1 :: [Int])) -> conjoin $ map (\(el, rem) -> insert el rem === l1) (picks l1)
    describe "permutations" $ -- only small tests because of factorial time complexity
        it "Generates all permutations of elements" $
            conjoin $ map (\n -> sort (Data.List.permutations [0..n]) === sort (Utils.List.permutations [0..n])) [0..7]