{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module RedBlackTree where


data NaturalNum = Z
                | S NaturalNum

-- data Color = Red | Black

data Color :: * where
  Red :: Color
  Black :: Color

-- Equivalent
-- data List a = Nada | Something a
data Possibly :: * -> * where
  Nada :: Possibly a
  Something :: a -> Possibly a

foobar False = Nada
foobar True = Something True



-- data Tree c n a = Empty Black Z a
--                 | RTree (Tree Black n a) a (Tree Black n a)
--                 | BTree (lc n a) a (Tree rc n a)


-- our core
data Tree :: Color -> NaturalNum -> * -> * where
  Empty :: Tree Black Z a
  RTree :: Tree Black natNum a     -> a -> Tree Black natNum a      -> Tree Red natNum a
  BTree :: Tree leftColor natNum a -> a -> Tree rightColor natNum a -> Tree Black (S natNum) a


data RedBlackTree :: * -> * where
  Root :: Tree Black natNum a -> RedBlackTree a


data MaybeRedRootTree :: NaturalNum -> * -> * where
  MaybeRedRoot :: ColorTypeValue c -> Tree cL n a -> a -> Tree cR n a -> MaybeRedRootTree (MaybeIncrement c n) a


-- This is so we can write functions which do not care what the color of the
-- tree is. Without this, we would have an explosion on the number of functions
-- which deal with either red or black trees. We do not want to throw out the
-- color information, so it is put in the constructior so we can still pattern
-- match against it.
data AbsorbColorTree :: NaturalNum -> * -> * where
  AbsorbRed   :: Tree Red n a -> AbsorbColorTree n a
  AbsorbBlack :: Tree Black n a -> AbsorbColorTree n a



-- We need this because we need to be able to create new NaturalNum types
-- without enforcing the color constraints. We very specifically want
-- MaybeRedRootTree to have only one constructor. 'type family' allows us to
-- pattern match against the types.
type family MaybeIncrement (c::Color) (n::NaturalNum) :: NaturalNum where
  MaybeIncrement Red x = x
  MaybeIncrement Black x = S x


-- We need this because we need to be able to pass colors as values. There is no
-- way (that I know of) to populate our types within our Color kind, and even if
-- we could, we think that you need to pass in values with a type which is in
-- the * kind to any valid function.
data ColorTypeValue :: Color -> * where
  RedTypeValue :: ColorTypeValue Red
  BlackTypeValue :: ColorTypeValue Black



contains :: (Ord a) => RedBlackTree a -> a -> Bool

contains (Root t) = treeContains t


treeContains :: (Ord a) => Tree c n a -> a -> Bool

treeContains Empty _ = False

treeContains (RTree left curr right) dat
  | dat == curr = True
  | dat < curr  = treeContains left dat
  | otherwise   = treeContains right dat

treeContains (BTree left curr right) dat
  | dat ==  curr = True
  | dat < curr   = treeContains left dat
  | otherwise    = treeContains right dat


-- This is impossible for really difficult to understand reasons. You lose the
-- type safety if you write a function which can return any of the types of our
-- GADTs. The you cannot check type safety if you do not know the return type of
-- a function.
-- findSubtree Empty _ = Empty
-- findSubtree (RTree left curr right) item
--   | curr == item = RTree left curr right
--   | curr < item  = findSubtree left item
--   | otherwise    = findSubtree right item
-- findSubtree (BTree left curr right) item
--   | curr == item = BTree left curr right --   | curr < item  = findSubtree left item
--   | otherwise    = findSubtree right item


-- non rotation cases. Just convert between types of trees
redBalanceLeft :: AbsorbColorTree n a -> a -> Tree c n a -> MaybeRedRootTree n a
redBalanceLeft (AbsorbRed left) item right =
  MaybeRedRoot RedTypeValue left item right
redBalanceLeft (AbsorbBlack left) item right =
  MaybeRedRoot RedTypeValue left item right

redBalanceRight :: Tree c n a -> a -> AbsorbColorTree n a -> MaybeRedRootTree n a
redBalanceRight left item (AbsorbRed right) =
  MaybeRedRoot RedTypeValue left item right
redBalanceRight left item (AbsorbBlack right) =
  MaybeRedRoot RedTypeValue left item right

-- Rotation cases. Have fun tracing this.
blackBalanceLeft :: MaybeRedRootTree n a -> a -> Tree c n a -> AbsorbColorTree (S n) a
blackBalanceLeft (MaybeRedRoot RedTypeValue (RTree lll llx llr) lx lr) x right =
  AbsorbRed $ RTree (BTree lll llx llr) lx (BTree lr x right)

blackBalanceLeft (MaybeRedRoot RedTypeValue ll lx (RTree lrl lrx lrr)) x right =
  AbsorbRed $ RTree (BTree ll lx lrl) lrx (BTree lrr x right)

blackBalanceLeft (MaybeRedRoot BlackTypeValue ll lx lr) x right =
  AbsorbBlack $ BTree (BTree ll lx lr) x right

-- We need to know that this is a black tree, and we cant match BTree to Empty,
-- so we need two separate black cases
blackBalanceLeft (MaybeRedRoot RedTypeValue Empty lx Empty) x right =
  AbsorbBlack $ BTree (RTree Empty lx Empty) x right

blackBalanceLeft (MaybeRedRoot RedTypeValue (BTree lll llx llr) lx (BTree lrl lrx lrr)) x right =
  AbsorbBlack $ BTree (RTree (BTree lll llx llr) lx (BTree lrl lrx lrr)) x right

blackBalanceRight :: Tree c n a -> a -> MaybeRedRootTree n a -> AbsorbColorTree (S n) a
blackBalanceRight left x (MaybeRedRoot RedTypeValue rl rx (RTree rrl rrx rrr)) =
  AbsorbRed $ RTree (BTree left x rl) rx (BTree rrl rrx rrr)

blackBalanceRight left x (MaybeRedRoot RedTypeValue (RTree rll rlx rlr) rx rr) =
  AbsorbRed $ RTree (BTree left x rll) rlx (BTree rlr rx rr)

blackBalanceRight left x (MaybeRedRoot BlackTypeValue rl rx rr) =
  AbsorbBlack $ BTree left x (BTree rl rx rr)

blackBalanceRight left x (MaybeRedRoot RedTypeValue Empty rx Empty) =
  AbsorbBlack $ BTree left x (RTree Empty rx Empty)

blackBalanceRight left x (MaybeRedRoot RedTypeValue (BTree rll rlx rlr) rx (BTree rrl rrx rrr)) =
  AbsorbBlack $ BTree left x (RTree (BTree rll rlx rlr) rx (BTree rrl rrx rrr))



-- treeInsert :: Tree c n a -> a -> MaybeRedRootTree n a
-- treeInsert Empty item =
--   MaybeRedRoot RedTypeValue Empty item Empty

--   -- RTree :: Tree Black natNum a     -> a -> Tree Black natNum a      -> Tree Red natNum a
-- treeInsert (BTree left curr right) item
--   | item < curr = MaybeRedRoot BlackTypeValue (treeInsert left item) curr right
