module BentleyOttmannSpec ( testIntersection )
where

import Data.List
import BentleyOttmann
import Test.HUnit

testIntersection :: [Test]
testIntersection =
  map (TestCase . assertBool "Bentley-Ottmann failed")

  -- testing basic case
  [ [(0.0,0.0)] == findIntersections [ constructLineSeg (-1, 0) (1, 0)
                                     , constructLineSeg (-1, -1) (1, 1) ]

  -- testing that the order does not matter
  , [(0.0,0.0)] == findIntersections [ constructLineSeg (1, 0) (-1, 0)
                                     , constructLineSeg (-1, -1) (1, 1) ]

  -- slightly more complicated case, still one intersection
  , [(0.0,0.0)] == findIntersections [ constructLineSeg (-1, 0) (1, 0)
                                     , constructLineSeg (-1, -1) (1, 1)
                                     , constructLineSeg (-1, -1) (1, 1) ]

  , sort [(-1.0,0.0),(2.0,0.0),(0.0, 2.0)]
      == sort (findIntersections [ constructLineSeg (-2,-2) (0.5,3)
                                 , constructLineSeg (-4,0) (4,0)
                                 , constructLineSeg (-3,5) (3,-1) ])

  -- testing a 'no intersection' case
  , [] == findIntersections [ constructLineSeg (1,1) (2,2)
                            , constructLineSeg (-2,-2) (-1,0) ]

  ]
