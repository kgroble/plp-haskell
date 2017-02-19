module RedBlackTreeSpec ( testContains
                        , testRemove
                        , testInsert )
where

import RedBlackTree as RBT
import Test.HUnit
import Data.List

testTree1 :: RedBlackTree Int
testTree1 = foldl (flip insertRBT) emptyRBT [1,2..10]

testTree2 :: RedBlackTree Int
testTree2 = foldl (flip insertRBT) emptyRBT [12,25,37,50,63,75,88,100]


testInsert :: [Test]
testInsert =
  map (TestCase . assertBool "Failed on an 'insert' test")
  [ [1] == toList (insertRBT 1 emptyRBT)
  , [1,2,3] == toList (foldl (flip insertRBT) emptyRBT [1,2,3])
  , [1,2..1000] == toList (foldl (flip insertRBT) emptyRBT [1,2..1000])
  , [1,5..1000] == toList (foldl (flip insertRBT) emptyRBT [1,5..1000])
  , sort [1000,999..0] == toList (foldl (flip insertRBT) emptyRBT [1000,999..0])
  , sort [1000,995..0] == toList (foldl (flip insertRBT) emptyRBT [1000,995..0])
  , sort [1000,995..0] /= toList (foldl (flip insertRBT) emptyRBT [1000,995..1])
  ]


testRemove :: [Test]
testRemove =
  map (TestCase . assertBool "Failed on an 'remove' test")
  [ [] == toList (remove 1 emptyRBT)
  , [] == toList (remove 1 (insertRBT 1 emptyRBT))
  , [1] == toList (foldl (flip remove) (foldl (flip insertRBT) emptyRBT [1,2..100]) [2,3..100])
  ]


testContains :: [Test]
testContains =
  map (TestCase . assertBool "Failed on a 'contains' test")

  [ all (`contains` testTree1) [1,2..10]
  , not $ contains 0 testTree1
  , not $ contains 100 testTree1
  , not $ contains 0 testTree2
  , not $ contains 24 testTree2
  , not $ contains 40 testTree2
  , not $ contains 55 testTree2
  , not $ contains 80 testTree2
  , not $ contains 90 testTree2
  , contains 12 testTree2
  , contains 25 testTree2
  , contains 50 testTree2
  , contains 88 testTree2
  ]
