module Utils.List
    ( reduceList
    , combinations
    , combinations2
    , powerset
    , nubSorted
    , splitOn
    , allEqual
    , uniquesBy
    , uniques
    , updateAt
    , deleteAt
    , takeLast
    , ascendingSum
    , descendingSum
    , ascendingIntersection
    , descendingIntersection
    , ascendingMinus
    , descendingMinus
    , ascendingXor
    , descendingXor
    , foldDescend
    , everyOther
    , interweave
    , picks
    , permutations
    , maxBy
    ) where
import Data.List(groupBy, maximumBy, sort)
import Data.Function(on)

-- Given list of elements, reduce it to pair (first element, length).
-- Usually this is used to reduce list with all elements equal.
reduceList :: [a] -> (a, Int)
reduceList [] = (error "Utils.List.reduceList: reducing empty list", 0)
reduceList (h:t) = (h, length t + 1)

-- Given natural number n and list of elements l, create list consisting of all
-- possible choices of n elements from list l.
combinations :: (Num t, Eq t) => t -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs

-- Just like combinations, but instead of list of selected elements
-- function returns list of pairs: selected + rejected elements.
combinations2 :: (Num t, Eq t) => t -> [a] -> [([a], [a])]
combinations2 0 l = [([], l)]
combinations2 _ [] = []
combinations2 n (x:xs) = map (\(sel, rej) -> (x:sel, rej)) (combinations2 (n - 1) xs)
                      ++ map (\(sel, rej) -> (sel, x:rej)) (combinations2 n xs)

-- Create list of all subsets of list.
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (h:t) = let ps = powerset t in ps ++ map (h:) ps

-- Remove consecutive duplicates from list.
nubSorted :: (Eq a) => [a] -> [a]
nubSorted [] = []
nubSorted [a] = [a]
nubSorted (h:n:t) = let t' = nubSorted (n:t) in if h == n then t' else h:t'

-- Split list l into list of lists, separating whenever element e appears.
-- Empty lists can appear in output list if e appears in list multiple times in a row
-- or it is last element of input string.
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn e l = case span (/= e) l of
    (s, []) -> [s]
    (s, h:t) -> s : splitOn e t

-- Check if all elements in list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual (x:y:z) = x == y && allEqual (y:z)
allEqual _ = True

-- Sort list and remove duplicates based on provided function.
uniquesBy :: (Eq a, Ord a) => (a -> a -> Bool) -> [a] -> [a]
uniquesBy fn = map head . groupBy fn . sort

-- Sort list and remove duplicates.
uniques :: (Eq a, Ord a) => [a] -> [a]
uniques = uniquesBy (==)

-- Update nth element of list using function f (if n >= length l, nothing happens)
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 f (h:t) = f h : t
updateAt n f (h:t) = h : updateAt (n - 1) f t

-- Delete nth element from list l (if n >= length l, nothing happens).
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (h:t) = t
deleteAt n (h:t) = h : deleteAt (n - 1) t

-- Find last element e in list such that e and every element before e satisfies predicate.
-- Function returns Nothing if list is empty or first element doesn't satisfy predicate.
takeLast :: (a -> Bool) -> [a] -> Maybe a
takeLast pred ls =
    let takeLast' acc [] = acc
        takeLast' acc (el:els)
            | pred el = takeLast' (Just el) els
            | otherwise = acc
    in takeLast' Nothing ls

-- Given two sorted ascending lists, take their sum.
-- It is expected for lists to have no duplicates.
ascendingSum :: Ord a => [a] -> [a] -> [a]
ascendingSum (a:x) (b:y) = case compare a b of
    LT -> a : ascendingSum x (b:y)
    EQ -> a : ascendingSum x y
    GT -> b : ascendingSum (a:x) y
ascendingSum [] l = l
ascendingSum l [] = l

-- Given two sorted descending lists, take their sum.
-- It is expected for lists to have no duplicates.
descendingSum :: Ord a => [a] -> [a] -> [a]
descendingSum (a:x) (b:y) = case compare a b of
    GT -> a : descendingSum x (b:y)
    EQ -> a : descendingSum x y
    LT -> b : descendingSum (a:x) y
descendingSum [] l = l
descendingSum l [] = l

-- Given two sorted ascending lists, take their intersection.
-- It is expected for lists to have no duplicates.
ascendingIntersection :: Ord a => [a] -> [a] -> [a]
ascendingIntersection (a:x) (b:y) = case compare a b of
    LT -> ascendingIntersection x (b:y)
    EQ -> a : ascendingIntersection x y
    GT -> ascendingIntersection (a:x) y
ascendingIntersection _ _ = []

-- Given two sorted descending lists, take their intersection.
-- It is expected for lists to have no duplicates.
descendingIntersection :: Ord a => [a] -> [a] -> [a]
descendingIntersection (a:x) (b:y) = case compare a b of
    GT -> descendingIntersection x (b:y)
    EQ -> a : descendingIntersection x y
    LT -> descendingIntersection (a:x) y
descendingIntersection _ _ = []

-- Given two sorted ascending lists, take their difference.
-- It is expected for lists to have no duplicates.
ascendingMinus :: Ord a => [a] -> [a] -> [a]
ascendingMinus (a:x) (b:y) = case compare a b of
    LT -> a : ascendingMinus x (b:y)
    EQ -> ascendingMinus x y
    GT -> ascendingMinus (a:x) y
ascendingMinus l [] = l
ascendingMinus [] _ = []

-- Given two sorted descending lists, take their intersection.
-- It is expected for lists to have no duplicates.
descendingMinus :: Ord a => [a] -> [a] -> [a]
descendingMinus (a:x) (b:y) = case compare a b of
    GT -> a : descendingMinus x (b:y)
    EQ -> descendingMinus x y
    LT -> descendingMinus (a:x) y
descendingMinus l [] = l
descendingMinus [] _ = []

-- Given two sorted ascending lists, take their symmetrical difference (xor).
-- It is expected for lists to have no duplicates.
ascendingXor :: Ord a => [a] -> [a] -> [a]
ascendingXor (a:x) (b:y) = case compare a b of
    LT -> a : ascendingXor x (b:y)
    EQ -> ascendingXor x y
    GT -> b : ascendingXor (a:x) y
ascendingXor l [] = l
ascendingXor [] l = l

-- Given two sorted descending lists, take their symmetrical difference (xor).
-- It is expected for lists to have no duplicates.
descendingXor :: Ord a => [a] -> [a] -> [a]
descendingXor (a:x) (b:y) = case compare a b of
    GT -> a : descendingXor x (b:y)
    EQ -> descendingXor x y
    LT -> b : descendingXor (a:x) y
descendingXor l [] = l
descendingXor [] l = l

-- Given list of elements and function taking two arguments, apply function
-- to pairs of consecutive elements.
foldDescend :: (a -> a -> b) -> [a] -> [b]
foldDescend f = go where
    go (x:y:t) = f x y : go (y:t)
    go _ = []

-- Take every other element of list. First parameter tells whether to take first element.
everyOther :: Bool -> [a] -> [a]
everyOther _ [] = []
everyOther True (h:t) = h : everyOther False t
everyOther False (h:t) = everyOther True t

-- Interweave two lists, starting with the first element of the first list.
interweave :: [a] -> [a] -> [a]
interweave (x:xs) (y:ys) = x : y : interweave xs ys
interweave l [] = l
interweave [] l = l

-- Given list l, generate all pairs (x, l') such that x is from l and l' is l without x.
picks :: [a] -> [(a, [a])]
picks (h:n:t) = (h, n:t) : map (fmap (h:)) (picks $ n:t)
picks [h] = [(h, [])]
picks [] = []

-- Generate list of all permutations of list. The difference between this function and Data.List.permutations
-- is that all permutations are generated in lexicographical order (assuming that initial input is the first one).
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations l = [h:t | (h, ts) <- picks l, t <- permutations ts]

-- Combination of maximumBy, compare and on functions.
maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy f = maximumBy (compare `on` f)