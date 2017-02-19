module Queue where

data Queue a = Empty | Queue Int (Queue a) a (Queue a) deriving Show

--find the rank of the queue
rank :: Queue a -> Int
rank Empty = 0
rank (Queue r _ _ _) = r

--empty queue
empty :: Queue a
empty = Empty

--single queue used for inserts
single :: Ord a => a -> Queue a
single a = Queue 1 Empty a Empty

insert :: Ord a => a -> Queue a ->  Queue a
insert a queue = merge  (single a) queue

merge :: Ord a => Queue a -> Queue a -> Queue a
merge Empty t = t
merge t Empty = t
merge t1@(Queue rank1 left1 a1 right1) t2@(Queue rank2 left2 a2 right2) 
	| a1 <= a2 = swap a1 left1 (merge right1 t2)
	| otherwise = swap a2 left2 (merge right2 t1)

swap :: a -> Queue a  -> Queue a -> Queue a
swap a t1 t2 
	| rank t1 < rank t2 = Queue (rank t1 + 1) t2 a t1
	| otherwise = Queue (rank t2 + 1) t1 a t2

--getMin ::  Queue a -> Int
getMin Empty = 0
getMin (Queue _ _  a _) = a

deleteMin :: Ord a => Queue a -> Queue a
deleteMin Empty = Empty
deleteMin (Queue _ left _ right) = 
	merge left right

