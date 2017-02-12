import Data.List

data MyList a = Empty | MyList a (MyList a)

fromList :: [a] -> MyList a
fromList [] = Empty
fromList (x:rest) = MyList x (fromList rest)
-------------------------------------------------
printList :: (Show a) => MyList a -> String
printList aList = "[" ++ (printListHelper aList "") ++ "]"

printListHelper :: (Show a) => MyList a -> String -> String
printListHelper Empty soFar = soFar
printListHelper (MyList x rest) soFar = printListHelper rest (soFar ++ " " ++ (show x))
-------------------------------------------------
myLength :: MyList a -> Int
myLength Empty = 0
myLength (MyList _ rest) = 1 + myLength rest
-------------------------------------------------
myReverse :: MyList a -> MyList a
myReverse Empty = Empty
myReverse aList = myReverseHelper aList Empty

myReverseHelper :: MyList a -> MyList a -> MyList a
myReverseHelper Empty soFar = soFar
myReverseHelper (MyList x rest) soFar = myReverseHelper rest (MyList x soFar)
-------------------------------------------------
mySort1 :: (Ord a) => MyList a -> MyList a
mySort1 aList =
    let realList = toList aList
        sorted = sort realList
    in fromList sorted

toList :: MyList a -> [a]
toList Empty = []
toList (MyList f rest) = (f:toList rest)

-------------------------------------------------
mySort2 :: (Ord a) => MyList a -> MyList a
mySort2 aList = sortHelper aList Empty

sortHelper :: (Ord a) => MyList a -> MyList a -> MyList a
sortHelper Empty soFar = soFar
sortHelper (MyList x rest) soFar = sortHelper rest (insertSorted x soFar)

insertSorted :: (Ord a) => a -> MyList a -> MyList a
insertSorted x Empty = MyList x Empty
insertSorted x (MyList y rest) =
    if x < y then
        MyList x (MyList y rest)
    else
        MyList y (insertSorted x rest)
-------------------------------------------------
main =
    --let x = MyList 2 (MyList 1 (MyList 3 Empty))
    let x = fromList [2, 1, 3, 4, 8, 6, 4]
    in putStrLn $  "Original: " ++ (printList x)
                   ++ "\nLength: " ++ (show (myLength x))
                   ++ "\nReversed: " ++ (printList (myReverse x))
                   ++ "\nSorted: " ++ (printList (mySort2 x))
