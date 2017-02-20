module QueueSpec ( testQueue )
where

import Data.List
import Queue
import Test.HUnit

testQueue :: [Test]
testQueue =
  map (TestCase . assertBool "Queue failed")
  [ 
  --test insert
  member 1 (insert 1 (listToQueue [4,6,87,3])),
  member 5 (insert 5 (listToQueue [4,6,87,3,22,48])),
  member 17 (insert 17 (listToQueue [4,6,87,3,22,48])),

  -- test getMin
  1 == getMin (listToQueue [4,3,5,6,1]),
  3 == getMin (delete 1 (listToQueue [4,3,5,6,1])),
  Nothing == getMin Empty,
  
  --test deleteMin
  (4, _ ) == deleteMin (listToQueue [7,8,11,4]),
  (member 8 (deleteMin (listToQueue [7,8,11,4]))),
  (member 7 (deleteMin (listToQueue [7,8,11,4]))),
  (member 11 (deleteMin (listToQueue [7,8,11,4]))),

  --test delete function
  not (member 5 (delete 5 (listToQueue [4,3,5,6,1]))),
  not (member 6 (delete 6 (listToQueue [4,3,5,6,1]))),
  not (member 4 (delete 4 (listToQueue [4,3,5,6,1])))
  ]