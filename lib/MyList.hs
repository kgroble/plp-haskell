import Data.List

-- A List of a's is either empty or an a (head) and a List of a's (tail)
data MyList a = Empty | MyList a (MyList a)

-- Take a real Haskell list and turn it into a MyList
fromList :: [a] -> MyList a
fromList [] = Empty
fromList (x:rest) = MyList x (fromList rest)
-------------------------------------------------
-- 2 pts
-- Return a reasonable string representation of the given list.
-- You don't have to worry about extra spaces or commas floating around.

-- Note the (Show a) constraint. If you pass a list of type a to this, a must derive show.
-- To get a string representation of element e of type a, call (show e).
printList :: (Show a) => MyList a -> String
printList _ = ""
-------------------------------------------------
-- 2 pts
-- Return the length of the given list.
myLength :: MyList a -> Int
myLength _ = 0
-------------------------------------------------
-- 2 pts
-- Return the reverse of the given list.
myReverse :: MyList a -> MyList a
myReverse _ = Empty

-------------------------------------------------
-- 4 pts
{-
  Return a list with the same elements as the given list in sorted order.
  It's a little tricky to write a proper recusrive definition for this,
  so you can get 3 of the 4 points for just writing toList (the opposite of fromList),
  then composing the built in sort (just called sort), fromList, and toList.
  For full credit, come up with a solution that deals purely with MyLists.
-}

-- NOTE: You will have to modify this type signature.
-- How do you know if you can sort a list of type a?
mySort :: MyList a -> MyList a
mySort _ = Empty

-------------------------------------------------
main =
    let x = fromList [2, 1, 3, 4, 8, 6, 4]
    in putStrLn $  "Original: " ++ (printList x)
                   ++ "\nLength: " ++ (show (myLength x))
                   ++ "\nReversed: " ++ (printList (myReverse x))
                   ++ "\nSorted: " ++ (printList (mySort x))
