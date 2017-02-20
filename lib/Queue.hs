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
single :: a -> Queue a
single a = Queue 1 Empty a Empty

insert :: Ord a => a -> Queue a ->  Queue a
insert a = merge (single a)

merge :: Ord a => Queue a -> Queue a -> Queue a
merge Empty t = t
merge t Empty = t
merge t1@(Queue _ left1 a1 right1) t2@(Queue _ left2 a2 right2)
  | a1 <= a2 = swap a1 left1 (merge right1 t2)
  | otherwise = swap a2 left2 (merge right2 t1)

swap :: a -> Queue a  -> Queue a -> Queue a
swap a t1 t2
  | rank t1 < rank t2 = Queue (rank t1 + 1) t2 a t1
  | otherwise = Queue (rank t2 + 1) t1 a t2

getMin :: Queue a -> Maybe a
getMin Empty = Nothing
getMin (Queue _ _ a _) = Just a

deleteMin :: Ord a => Queue a -> Maybe (a, Queue a)
deleteMin q =
  case getMin q of
    Nothing -> Nothing
    Just x -> Just (x, delete x q)
-- deleteMin Empty = Nothing
-- deleteMin (Queue _ left _ right) =
--   merge left right

rebalance :: Queue a -> Queue a 
rebalance (Queue _ left a right)
  | rank left < rank right = Queue (rank left + 1) right a left
  | otherwise = Queue (rank right + 1) left a right 
rebalance Empty = Empty

deleteHelp :: Ord a => a -> Queue a -> Queue a
deleteHelp _ Empty = Empty
deleteHelp i (Queue r left a right) 
  | i == a = merge left right
  | otherwise = Queue r (deleteHelp i left) a (deleteHelp i right)

delete :: Ord a => a -> Queue a -> Queue a
delete a queue = rebalance (deleteHelp a queue) 
