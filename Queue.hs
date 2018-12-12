module Queue
    ( Queue
    , new
    , enqueue
    , enqueueSingle
    , dequeue
    , empty
    , toList
    , fromList
    ) where


{-
    Alright, so what's the big picture here?

    Implement a basic queue ADT, then force provers to use it. That should
    clarify things and help draw out common patterns. After that we can revisit
    abstraction, as well as standard implementations of queues (or some other
    useful datatype).

    I just looked it up, and there's literally a Data.Queue. Ah well, I need to
    learn this stuff anyway. It's good practice!
-}

newtype Queue a = Queue [a]

new :: Queue a
new = Queue []

toList :: Queue a -> [a]
toList (Queue q) = q

fromList :: [a] -> Queue a
fromList xs = enqueue xs new

empty :: Queue a -> Bool
empty (Queue q) = null q

enqueue :: [a] -> Queue a -> Queue a
enqueue xs (Queue q) = Queue (q ++ xs)

enqueueSingle :: a -> Queue a -> Queue a
enqueueSingle x = enqueue [x]

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue []) = Nothing
dequeue (Queue (x:xs)) = Just (x, Queue xs)
