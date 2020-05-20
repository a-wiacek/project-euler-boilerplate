module Utils.Queue
    ( Queue
    , reorder
    , empty
    , singleton
    , fromList
    , toList
    , size
    , enqueue
    , peek
    , dequeue
    ) where

-- Simple implementation of queue using two lists.

data Queue a = Queue Int [a] [a]

-- Eq is not derived, since two queues should be equal if they keep the same elements
-- in the same order. They don't nned to have the same internal representations.
instance Eq a => Eq (Queue a) where
    q1 == q2 = size q1 == size q2 && toList q1 == toList q2

instance Show a => Show (Queue a) where
    showsPrec d q = showParen (d > 10) $ showString "fromList " . shows (toList q)

instance Functor Queue where
    fmap f (Queue n b e) = Queue n (map f b) (map f e)

instance Foldable Queue where
    foldr f v = foldr f v . toList

instance Semigroup (Queue a) where
    (Queue n1 b1 e1) <> (Queue n2 b2 e2) = Queue (n1 + n2) b1 (e2 ++ reverse b2 ++ e1)

instance Monoid (Queue a) where
    mempty = empty

-- Move all elements to second list.
reorder :: Queue a -> Queue a
reorder (Queue n l1 l2) = Queue n [] (l2 ++ reverse l1)

-- Create empty queue.
empty :: Queue a
empty = Queue 0 [] []

-- Create queue with single element.
singleton :: a -> Queue a
singleton e = Queue 1 [] [e]

-- Create queue from list.
fromList :: [a] -> Queue a
fromList l = Queue (length l) [] (reverse l)

-- Get list of elements in queue. Element on the left is the most recently enqueued.
toList :: Queue a -> [a]
toList (Queue _ l1 l2) = l1 ++ reverse l2

-- Get amount of elements in queue.
size :: Queue a -> Int
size (Queue n _ _) = n

-- Put element at the end of queue.
enqueue :: a -> Queue a -> Queue a
enqueue e (Queue n l1 l2) = Queue (n + 1) (e:l1) l2

-- Peek value at the top of queue. Returns Just element or Nothing if queue is empty and modified queue.
-- Note that peek returns pair instead of just an element. If second list in queue
-- is empty, we are modifying its internal state by reordering it.
peek :: Queue a -> (Maybe a, Queue a)
peek q@(Queue n _ l2)
    | n == 0 = (Nothing, q)
    | null l2 = peek (reorder q)
    | otherwise = (Just (head l2), q)

-- Pop element from queue. Returns Just element or Nothing if queue is empty and modified queue.
dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Queue n l1 l2)
    | n == 0 = (Nothing, q)
    | null l2 = dequeue (reorder q)
    | otherwise = (Just (head l2), Queue (n - 1) l1 (tail l2))