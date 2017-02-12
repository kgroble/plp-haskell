{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

module Main where

import HTMLEscaping
-- import RedBlackTree

class Pretty a where
  display :: a -> String

data MyList a = Empty
              | Something a (MyList a)

instance (Pretty a) => Pretty (MyList a) where
  display Empty = ""
  display (Something a rest) = (display a) ++ (display rest)



instance Pretty Int where
  display x = show x

main :: IO ()
main =
  let
    s = getString "Hello"
  in putStrLn $ convertString s








instance Pretty Integer where
  display x = show x

instance Pretty Float where
  display x = show x

instance Pretty Color where
  display Red = "Red "
  display Blue = "Blue "
  display Green = "Green "
  display White = "White "

data Color = Red
           | Blue
           | Green
           | White
  -- putStrLn $ display $ Something Red $ Something Blue $ Something Green $ Something Red Empty

-- data Something = A | B | C

-- main :: IO ()
-- main =
--   let tree = Root (BTree (Empty) A (RTree (Empty) B (Empty)))
--       firstTest = contains tree A
--       another = contains tree B
--       ah = contains tree C
--   in
--     putStrLn (if (firstTest && another && (not ah))
--               then "All passed!!!!!"
--               else "damn")
