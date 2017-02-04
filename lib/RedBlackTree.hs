{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
module RedBlackTree where

data NaturalNum = Z | S NaturalNum

-- Color is a type which has kind *
-- Red is a value with type Color
data Color :: * where
  Red :: Color
  Black :: Color


data Tree a :: Color -> NaturalNum -> * where
  Empty :: Tree a Black Z
  RTree :: Tree a Black natNum -> a -> Tree a Black natNum -> Tree a Red natNum
  BTree :: Tree a leftColor natNum -> a -> Tree a rightColor natNum -> Tree a Black (S natNum)


data RedBlackTree a :: * where
  Root :: Tree a Black natNum -> RedBlackTree a


contains :: (Ord a) => RedBlackTree a -> a -> Bool
contains (Root t) datum =
  treeContains t datum


treeContains :: (Ord a) => Tree a c n -> a -> Bool
treeContains Empty dat = False

treeContains (RTree left curr right) dat =
  if dat == curr
  then True
  else if dat < curr
  then treeContains left dat
  else treeContains right dat

treeContains (BTree left curr right) dat =
  if dat == curr
  then True
  else if dat < curr
  then treeContains left dat
  else treeContains right dat
