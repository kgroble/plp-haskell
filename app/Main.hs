module Main where

import RedBlackTree

main :: IO ()
main =
  let tree = Root (BTree (Empty) 10 (RTree (Empty) 15 (Empty)))
      firstTest = contains tree 10
      another = contains tree 15
      ah = contains tree 16
  in
    putStrLn (if (firstTest && another && (not ah))
              then "All passed!!!!!"
              else "damn")
