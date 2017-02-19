{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- Used https://www.cis.upenn.edu/~sweirich/talks/typelevel16.pdf to help us code this

module RedBlackTree ( RedBlackTree
                    , insertRBT
                    , emptyRBT
                    , contains
                    , height
                    , remove
                    , pop
                    , toList ) where


-- ~~~~~~~~~~ Types ~~~~~~~~~~

data NaturalNum = Z
                | S NaturalNum
                deriving Show


data Color :: * where
  Red :: Color
  Black :: Color
  deriving Show

-- our core
data Tree :: Color -> NaturalNum -> * -> * where
  Empty :: Tree Black Z a
  RTree :: Tree Black natNum a     -> a -> Tree Black natNum a      -> Tree Red natNum a
  BTree :: Tree leftColor natNum a -> a -> Tree rightColor natNum a -> Tree Black (S natNum) a
deriving instance (Show a) => Show (Tree c n a)

data RedBlackTree :: * -> * where
  Root :: Tree Black natNum a -> RedBlackTree a
deriving instance (Show a) => Show (RedBlackTree a)




-- This is so we can write functions which do not care what the color of the
-- tree is. Without this, we would have an explosion on the number of functions
-- which deal with either red or black trees. We do not want to throw out the
-- color information, so it is put in the constructior so we can still pattern
-- match against it. Needs to be nonempty so the conversion to MaybeRedInsTrees
-- is valid
data AbsorbColorInsertTree :: NaturalNum -> * -> * where
  AbsorbRedInsert   :: Tree Red n a -> AbsorbColorInsertTree n a
  AbsorbBlackInsert :: Tree Black (S n) a -> AbsorbColorInsertTree (S n) a

data AbsorbColorRemoveTree :: NaturalNum -> * -> * where
  AbsorbRedRemove :: Tree Red n a -> AbsorbColorRemoveTree n a
  AbsorbBlackRemove :: Tree Black n a -> AbsorbColorRemoveTree n a


data MaybeRedInsTree :: NaturalNum -> * -> * where
  MaybeRedInsRoot :: ColorTypeValue c -> Tree cL n a -> a -> Tree cR n a -> MaybeRedInsTree (MaybeIncrement c n) a
  MaybeRedInsEmpty :: MaybeRedInsTree Z a

data MaybeRedRemTree :: NaturalNum -> * -> * where
  MaybeRedRemRoot :: ColorTypeValue c -> Tree cL n a -> a -> Tree cR n a -> MaybeRedRemTree (MaybeDecrement c n) a
  MaybeRedRemEmpty :: MaybeRedRemTree (S Z) a



-- We need this because we need to be able to create new NaturalNum types
-- without enforcing the color constraints. We very specifically want
-- MaybeRedInsTree to have only one constructor. 'type family' allows us to
-- pattern match against the types.
type family MaybeIncrement (c::Color) (n::NaturalNum) :: NaturalNum where
  MaybeIncrement Red x = x
  MaybeIncrement Black x = S x

type family MaybeDecrement (c::Color) (n::NaturalNum) :: NaturalNum where
  MaybeDecrement Red x = x
  MaybeDecrement Black (S x) = x


-- We need this because we need to be able to pass colors as values. There is no
-- way (that I know of) to populate our types within our Color kind, and even if
-- we could, we think that you need to pass in values with a type which is in
-- the * kind to any valid function.
data ColorTypeValue :: Color -> * where
  RedTypeValue :: ColorTypeValue Red
  BlackTypeValue :: ColorTypeValue Black






-- ~~~~~~~~~~ Utility Functions ~~~~~~~~~~

absorbColorToMaybeRedInsRoot :: AbsorbColorInsertTree n a -> MaybeRedInsTree n a
absorbColorToMaybeRedInsRoot (AbsorbRedInsert (RTree left x right)) =
  MaybeRedInsRoot RedTypeValue left x right
absorbColorToMaybeRedInsRoot (AbsorbBlackInsert (BTree left x right)) =
  MaybeRedInsRoot BlackTypeValue left x right


turnRootBlack :: MaybeRedInsTree n a -> RedBlackTree a
turnRootBlack (MaybeRedInsRoot _ left val right) =
  Root $ BTree left val right
turnRootBlack MaybeRedInsEmpty =
  Root Empty


insertIntoBlack :: (Ord a) => a -> Tree Black n a -> AbsorbColorInsertTree n a
insertIntoBlack item Empty =
  AbsorbRedInsert $ RTree Empty item Empty
insertIntoBlack item (BTree left curr right)
  | item < curr = blackBalanceLeft (insertTree item left) curr right
  | item > curr = blackBalanceRight left curr (insertTree item right)
  | otherwise = AbsorbBlackInsert (BTree left curr right)


insertIntoRed :: (Ord a) => a -> Tree Red n a -> MaybeRedInsTree n a
insertIntoRed item (RTree left curr right)
  | item < curr = redBalanceLeft (insertIntoBlack item left) curr right
  | item > curr = redBalanceRight left curr (insertIntoBlack item right)
  | otherwise = MaybeRedInsRoot RedTypeValue left curr right


insertTree :: (Ord a) => a -> Tree c n a -> MaybeRedInsTree n a
insertTree item Empty =
  absorbColorToMaybeRedInsRoot $ insertIntoBlack item Empty
insertTree item (BTree left curr right) =
  absorbColorToMaybeRedInsRoot $ insertIntoBlack item (BTree left curr right)
insertTree item (RTree left curr right) =
  insertIntoRed item (RTree left curr right)


treeContains :: (Ord a) => a -> Tree c n a -> Bool
treeContains _ Empty = False
treeContains dat (RTree left curr right)
  | dat == curr = True
  | dat < curr  = treeContains dat left
  | otherwise   = treeContains dat right
treeContains dat (BTree left curr right)
  | dat ==  curr = True
  | dat < curr   = treeContains dat left
  | otherwise    = treeContains dat right


treeToList :: Tree c n a -> [a]
treeToList Empty = []
treeToList (RTree left x right) =
  treeToList left ++ [x] ++ treeToList right
treeToList (BTree left x right) =
  treeToList left ++ [x] ++ treeToList right


treeHeight :: Tree c n a -> Int
treeHeight Empty = 0
treeHeight (BTree left _ right) =
  1 + max (treeHeight left) (treeHeight right)
treeHeight (RTree left _ right) =
  1 + max (treeHeight left) (treeHeight right)


treeSmallest :: (Monad m) => Tree c n a -> m a
treeSmallest (RTree Empty item _) = return item
treeSmallest (BTree Empty item _) = return item
treeSmallest (RTree left item _) = treeSmallest left
treeSmallest (BTree left item _) = treeSmallest left


-- blackTreeRemove :: (Ord a) => Tree Black n a -> a -> MaybeRedRemTree n a
-- blackTreeRemove (BTree Empty curr Empty) item
--   | item < curr = MaybeRedRemRoot BlackTypeValue Empty curr Empty

-- redTreeRemove :: (Ord a) => Tree Red n a -> a -> AbsorbColorRemoveTree n a
-- redTreeRemove (RTree Empty curr Empty) item
--   | curr == item = AbsorbBlackRemove Empty
--   | otherwise = AbsorbRedRemove $ RTree Empty curr Empty




-- treeRemoveSmallest :: Tree c n a -> (a, MaybeRedInsTree n a)





-- ~~~~~~~~~~ Rotations ~~~~~~~~~~

-- non rotation cases. Just convert between types of trees
redBalanceLeft :: AbsorbColorInsertTree n a -> a -> Tree c n a -> MaybeRedInsTree n a
redBalanceLeft (AbsorbRedInsert left) item right =
  MaybeRedInsRoot RedTypeValue left item right
redBalanceLeft (AbsorbBlackInsert left) item right =
  MaybeRedInsRoot RedTypeValue left item right

redBalanceRight :: Tree c n a -> a -> AbsorbColorInsertTree n a -> MaybeRedInsTree n a
redBalanceRight left item (AbsorbRedInsert right) =
  MaybeRedInsRoot RedTypeValue left item right
redBalanceRight left item (AbsorbBlackInsert right) =
  MaybeRedInsRoot RedTypeValue left item right

-- Rotation cases. Have fun tracing this.
blackBalanceLeft :: MaybeRedInsTree n a -> a -> Tree c n a -> AbsorbColorInsertTree (S n) a
blackBalanceLeft (MaybeRedInsRoot RedTypeValue (RTree lll llx llr) lx lr) x right =
  AbsorbRedInsert $ RTree (BTree lll llx llr) lx (BTree lr x right)

blackBalanceLeft (MaybeRedInsRoot RedTypeValue ll lx (RTree lrl lrx lrr)) x right =
  AbsorbRedInsert $ RTree (BTree ll lx lrl) lrx (BTree lrr x right)

blackBalanceLeft (MaybeRedInsRoot BlackTypeValue ll lx lr) x right =
  AbsorbBlackInsert $ BTree (BTree ll lx lr) x right

-- We need to know that this is a black tree, and we cant match BTree to Empty,
-- so we need two separate black cases
blackBalanceLeft (MaybeRedInsRoot RedTypeValue Empty lx Empty) x right =
  AbsorbBlackInsert $ BTree (RTree Empty lx Empty) x right

blackBalanceLeft (MaybeRedInsRoot RedTypeValue (BTree lll llx llr) lx (BTree lrl lrx lrr)) x right =
  AbsorbBlackInsert $ BTree (RTree (BTree lll llx llr) lx (BTree lrl lrx lrr)) x right

blackBalanceLeft _ _ _ = error "Impossible case"

blackBalanceRight :: Tree c n a -> a -> MaybeRedInsTree n a -> AbsorbColorInsertTree (S n) a
blackBalanceRight left x (MaybeRedInsRoot RedTypeValue rl rx (RTree rrl rrx rrr)) =
  AbsorbRedInsert $ RTree (BTree left x rl) rx (BTree rrl rrx rrr)

blackBalanceRight left x (MaybeRedInsRoot RedTypeValue (RTree rll rlx rlr) rx rr) =
  AbsorbRedInsert $ RTree (BTree left x rll) rlx (BTree rlr rx rr)

blackBalanceRight left x (MaybeRedInsRoot BlackTypeValue rl rx rr) =
  AbsorbBlackInsert $ BTree left x (BTree rl rx rr)

blackBalanceRight left x (MaybeRedInsRoot RedTypeValue Empty rx Empty) =
  AbsorbBlackInsert $ BTree left x (RTree Empty rx Empty)

blackBalanceRight left x (MaybeRedInsRoot RedTypeValue (BTree rll rlx rlr) rx (BTree rrl rrx rrr)) =
  AbsorbBlackInsert $ BTree left x (RTree (BTree rll rlx rlr) rx (BTree rrl rrx rrr))

blackBalanceRight _ _ _ = error "Impossible case"



-- ~~~~~~~~~~ Actual Functionality ~~~~~~~~~~

contains :: (Ord a) => a -> RedBlackTree a -> Bool
contains item (Root t) = treeContains item t


insertRBT :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
insertRBT item (Root t) =
  turnRootBlack $ insertTree item t


emptyRBT :: RedBlackTree a
emptyRBT = Root Empty


toList :: RedBlackTree a -> [a]
toList (Root t) =
  treeToList t


height :: RedBlackTree a -> Int
height (Root t) =
  treeHeight t


remove :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
remove item t = foldl (flip insertRBT) emptyRBT $ filter (/= item) $ toList t


pop :: (Ord a, Monad m) => RedBlackTree a -> m (a, RedBlackTree a)
pop rbt@(Root t) =
  let Just smallest = treeSmallest t
  in return (smallest, remove smallest rbt)
