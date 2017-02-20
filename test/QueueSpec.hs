module QueueSpec ( testQueue )
where

import Data.Maybe
import Queue
import Test.HUnit

testQueue :: [Test]
testQueue =
  map (TestCase . assertBool "Queue failed")
  [
    --test insert
    member 1  (Queue.insert 1 (listToQueue [4,6,87,3])),
    member 5  (Queue.insert 5 (listToQueue [4,6,87,3,22,48])),
    member 17 (Queue.insert 17 (listToQueue [4,6,87,3,22,48])),

    -- test getMin
    1 == fromJust (getMin (listToQueue ([4,3,5,6,1]::[Int]))),
    3 == fromJust (getMin (Queue.delete 1 (listToQueue ([4,3,5,6,1]::[Int])))),
    Nothing == getMin (Empty::Queue Int),

    -- test deleteMin
    (not (member 4 $ snd $ (fromJust $ deleteMin (listToQueue ([7,8,11,4]::[Int]))))),
    (member 8 $ snd $ fromJust (deleteMin (listToQueue [7,8,11,4]))),
    (member 7 $ snd $ fromJust (deleteMin (listToQueue [7,8,11,4]))),
    (member 11 $ snd $ fromJust (deleteMin (listToQueue [7,8,11,4]))),

    --test delete function
    not (member 5 (Queue.delete 5 (listToQueue [4,3,5,6,1]))),
    not (member 6 (Queue.delete 6 (listToQueue [4,3,5,6,1]))),
    not (member 4 (Queue.delete 4 (listToQueue [4,3,5,6,1])))
  ]
