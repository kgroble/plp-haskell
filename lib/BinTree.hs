module BinTree where

data BinTree a
  = N a (BinTree a) (BinTree a)
  | L
    deriving (Show, Eq)

null :: BinTree a -> Bool
null L = True
null _ = False

empty :: BinTree a
empty = L

fromList :: Ord a => [a] -> BinTree a
fromList = foldl (flip insert) L

insert :: Ord a => a -> BinTree a -> BinTree a
insert x L = N x L L
insert x (N x' t0 t1) | x <= x' = N x' (insert x t0) t1
                      | otherwise = N x' t0 (insert x t1)

delete :: Ord a => a -> BinTree a -> BinTree a
delete _ L = L
delete x (N x' t0 t1) | x < x' = N x' (delete x t0) t1
                      | x > x' = N x' t0 (delete x t1)
                      | x == x' = merge t0 t1
                      | otherwise = undefined
  where
    merge :: Ord a => BinTree a -> BinTree a -> BinTree a
    merge t L = t
    merge L t = t
    merge t (N _x _t0 _t1) = N _x t $ merge _t0 _t1


-- heap like interface (for BentleyOttmann x-structure)

viewMin :: Ord a => BinTree a -> Maybe (a, BinTree a)
viewMin t = f t >>= \x -> return (x, delete x t)
  where
    f :: BinTree a -> Maybe a
    f L = Nothing
    f (N x L _) = Just x
    f (N _ t' _) = f t'


-- BentleyOttmann y-structure interface

insertWith :: Ord b => (a -> b) -> a -> BinTree a -> BinTree a
insertWith _ x L = N x L L
insertWith f x (N x' t0 t1) | f x <= f x' = N x' (insertWith f x t0) t1
                             | otherwise = N x' t0 (insertWith f x t1)

deleteWith :: Ord b => (a -> b) -> a -> BinTree a -> Maybe (BinTree a)
deleteWith _ _ L = Nothing
deleteWith f x (N x' t0 t1) | f x < f x' = N x' <$> deleteWith f x t0 <*> pure t1
                             | f x > f x' = N x' t0 <$> deleteWith f x t1
                             | f x == f x' = Just $ merge f t0 t1
                             | otherwise = undefined
  where
    merge :: Ord b => (a -> b) -> BinTree a -> BinTree a -> BinTree a
    merge _ t L = t
    merge _ L t = t
    merge _f t (N _x _t0 _t1) = N _x t $ merge _f _t0 _t1

neighborsWith :: Ord b => (a -> b) -> a -> BinTree a -> (Maybe a, Maybe a)
neighborsWith _f _x _t = g _f _x _t (Nothing, Nothing)
  where
    g :: Ord b => (a -> b) -> a -> BinTree a -> (Maybe a, Maybe a) -> (Maybe a, Maybe a)
    g _ _ L _ = (Nothing, Nothing)
    g f x (N x' t0 _) (m, _) | f x < f x'  = g f x t0 (m, Just x')
    g f x (N x' _ t1) (_, m) | f x > f x'  = g f x t1 (Just x', m)
    g f x (N x' t0 t1) mm    | f x == f x' =
      case (t0, t1, mm) of
        (L, L, _     ) -> mm
        (_, L, (_, m)) -> (rightMost t0, m)
        (L, _, (m, _)) -> (m, leftMost t1)
        (_, _ , _    ) -> (rightMost t0, leftMost t1)
    g _ _ _ _ = undefined

leftMost :: BinTree a -> Maybe a
leftMost L = Nothing
leftMost (N x L _) = Just x
leftMost (N _ t _) = leftMost t

rightMost :: BinTree a -> Maybe a
rightMost L = Nothing
rightMost (N x _ L) = Just x
rightMost (N _ _ t) = rightMost t

-- assume x y exists in tree and f x == f y
swapWith :: (Eq a, Ord b) => (a -> b) -> a -> a -> BinTree a -> BinTree a
swapWith _ _ _ L = L
swapWith f x y (N z t0 t1) | x == z = N y (swapWith f x y t0) (swapWith f x y t1)
                            | y == z = N x (swapWith f x y t0) (swapWith f x y t1)
                            | f x < f z = N z (swapWith f x y t0) t1
                            | f z < f x = N z t0 (swapWith f x y t1)
                            | otherwise = undefined

