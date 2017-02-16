module BinaryTreeSpec ( containsTests )
where

import Test.HUnit
import BinaryTree

testTree1 :: Tree a
testTree1 = Empty

testTree2 :: Tree Int
testTree2 = foldl insert Empty [1, 2, 3, 4, 5]


containsTests :: [Test]
containsTests =

  map TestCase

  -- Empty tree test cases
  [ assertBool "" $ not $ contains testTree1 (1::Int)
  , assertBool "" $ not $ contains testTree1 (0::Int)
  , assertBool "" $ not $ contains testTree1 (-1::Int)

  -- Basic tree test cases
  , assertBool "" $ contains testTree2 (1::Int)
  , assertBool "" $ contains testTree2 (2::Int)
  , assertBool "" $ contains testTree2 (3::Int)
  , assertBool "" $ contains testTree2 (4::Int)
  , assertBool "" $ contains testTree2 (5::Int)
  , assertBool "" $ not $ contains testTree2 (6::Int)
  ]
