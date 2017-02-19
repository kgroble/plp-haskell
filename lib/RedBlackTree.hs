{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- Used https://www.cis.upenn.edu/~sweirich/talks/typelevel16.pdf to help us code this

module RedBlackTree ( RedBlackTree
                    , insertRBT
                    , emptyRBT
                    , contains
                    , height
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



data MaybeRedRootTree :: NaturalNum -> * -> * where
  MaybeRedRoot :: ColorTypeValue c -> Tree cL n a -> a -> Tree cR n a -> MaybeRedRootTree (MaybeIncrement c n) a


-- This is so we can write functions which do not care what the color of the
-- tree is. Without this, we would have an explosion on the number of functions
-- which deal with either red or black trees. We do not want to throw out the
-- color information, so it is put in the constructior so we can still pattern
-- match against it. Needs to be nonempty so the conversion to MaybeRedRootTrees
-- is valid
data AbsorbColorTree :: NaturalNum -> * -> * where
  AbsorbRed   :: Tree Red n a -> AbsorbColorTree n a
  AbsorbBlack :: Tree Black (S n) a -> AbsorbColorTree (S n) a



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






-- ~~~~~~~~~~ Utility Functions ~~~~~~~~~~

absorbColorToMaybeRedRoot :: AbsorbColorTree n a -> MaybeRedRootTree n a
absorbColorToMaybeRedRoot (AbsorbRed (RTree left x right)) =
  MaybeRedRoot RedTypeValue left x right
absorbColorToMaybeRedRoot (AbsorbBlack (BTree left x right)) =
  MaybeRedRoot BlackTypeValue left x right


turnRootBlack :: MaybeRedRootTree n a -> RedBlackTree a
turnRootBlack (MaybeRedRoot _ left val right) =
  Root $ BTree left val right


insertIntoBlack :: (Ord a) => a -> Tree Black n a -> AbsorbColorTree n a
insertIntoBlack item Empty =
  AbsorbRed $ RTree Empty item Empty
insertIntoBlack item (BTree left curr right)
  | item < curr = blackBalanceLeft (insertTree item left) curr right
  | item > curr = blackBalanceRight left curr (insertTree item right)
  | otherwise = AbsorbBlack (BTree left curr right)


insertIntoRed :: (Ord a) => a -> Tree Red n a -> MaybeRedRootTree n a
insertIntoRed item (RTree left curr right)
  | item < curr = redBalanceLeft (insertIntoBlack item left) curr right
  | item > curr = redBalanceRight left curr (insertIntoBlack item right)
  | otherwise = MaybeRedRoot RedTypeValue left curr right


insertTree :: (Ord a) => a -> Tree c n a -> MaybeRedRootTree n a
insertTree item Empty =
  absorbColorToMaybeRedRoot $ insertIntoBlack item Empty
insertTree item (BTree left curr right) =
  absorbColorToMaybeRedRoot $ insertIntoBlack item (BTree left curr right)
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




-- ~~~~~~~~~~ Rotations ~~~~~~~~~~

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
