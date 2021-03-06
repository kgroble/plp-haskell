module BentleyOttmann ( findIntersections
                      , LineSeg
                      , Point, X, Y
                      , constructLineSeg ) where

import Data.Function (fix)
import BinTree
import Queue

type X = Double
type Y = Double
type Point = (X, Y)

data LineSeg = LineSeg Point Point
  deriving (Eq, Show, Read)

data XEvent = LeftEndPoint LineSeg
            | RightEndPoint LineSeg
            | Intersection LineSeg LineSeg Point
           deriving (Show, Eq)


constructLineSeg :: Point -> Point -> LineSeg
constructLineSeg p1@(x1, _) p2@(x2, _)
  | x1 <= x2 = LineSeg p1 p2
  | otherwise = LineSeg p2 p1


extractX :: XEvent -> X
extractX (LeftEndPoint (LineSeg (x, _) _)) = x
extractX (RightEndPoint (LineSeg _ (x, _))) = x
extractX (Intersection _ _ (x, _)) = x

instance Ord XEvent where
  event1 < event2 = extractX event1 < extractX event2
  event1 <= event2 = event1 < event2 || event1 == event2

type EventQueue = Queue XEvent
type LineTree = BinTree LineSeg

loadEventQueue :: [LineSeg] -> EventQueue
loadEventQueue listOfLines = foldl (flip Queue.insert) Queue.empty (map LeftEndPoint listOfLines)

-- Nothing if two segments don't intersect.
-- Just their intersection point otherwise.
intersect :: LineSeg -> LineSeg -> Maybe Point
intersect (LineSeg (ax1, ay1) (ax2, ay2)) (LineSeg (bx1, by1) (bx2, by2)) =
  let
    aXDiff = ax2 - ax1
    aYDiff = ay2 - ay1
    bXDiff = bx2 - bx1
    bYDiff = by2 - by1
    aK = (aXDiff * ay1) - (aYDiff * ax1)
    bK = (bXDiff * by1) - (bYDiff * bx1)
    d = -(aYDiff * bXDiff) + (bYDiff * aXDiff)
    x = ((-aXDiff * bK) + (bXDiff * aK)) / d
    y = ((bYDiff * aK) - (aYDiff * bK)) / d
  in
    if d /= 0 && ax1 <= x && x <= ax2 && bx1 <= x && x <= bx2
    then Just (x, y)
    else Nothing

-- Similar: Intersect the vertical sweep line (x) with a segment.
intersectX :: LineSeg -> X -> Maybe Point
intersectX (LineSeg (x1, y1) (x2, y2)) x
  | x < x1 || x2 < x = Nothing
  | otherwise =
    let y = y1 + ((y2 - y1) / (x2 - x1)) * (x - x1)
    in Just (x, y)

-- Lines are compared by their y coordinate at the sweep line.
-- If they don't intersect the sweep line, arbitrary.
lineOrder :: X -> LineSeg -> Double
lineOrder x line =
  case intersectX line x of
    (Just (_, b)) -> b
    Nothing -> 0


-- Insert a line and return updated neighbor information.
insertLine :: X -> LineTree -> LineSeg -> (LineTree, [(LineSeg, LineSeg)], [(LineSeg, LineSeg)])
insertLine x tree line =
  let
    newTree = insertWith (lineOrder x) line tree
    (newNeighbors, newEnemies) =
      case neighborsWith (lineOrder x) line newTree of
        (Nothing, Nothing) -> ([], [])
        (Just small, Nothing) -> ([(line, small)], [])
        (Nothing, Just big) -> ([(big, line)], [])
        (Just small, Just big) -> ([(line, small), (big, line)], [(big, line)])
  in
    (newTree, newNeighbors, newEnemies)

-- Delete a line and return updated neighbor information.
deleteLine :: X -> LineTree -> LineSeg -> (LineTree,[(LineSeg, LineSeg)], [(LineSeg, LineSeg)])
deleteLine x tree line =
  let
    (newNeighbors, newEnemies) =
      case neighborsWith (lineOrder x) line tree of
        (Nothing, Nothing) -> ([], [])
        (Just small, Nothing) -> ([(line, small)], [])
        (Nothing, Just big) -> ([(big, line)], [])
        (Just small, Just big) -> ([(line, small), (big, line)], [(big, big)])
    newTree = case deleteWith (lineOrder x) line tree of
      (Just t) -> t
      Nothing -> error "removed from empty tree"
  in
    (newTree, newNeighbors, newEnemies)

-- Swap two lines (because the sweep line has moved past theis intersection).
-- Return updated neighbor information.
swapLines :: X -> LineTree -> LineSeg -> LineSeg -> (LineTree,[(LineSeg, LineSeg)], [(LineSeg, LineSeg)])
swapLines x tree line1 line2 =
  let
    (newNeighbors, newEnemies) =
      case (fst $ neighborsWith (lineOrder x) line2 tree, snd $ neighborsWith (lineOrder x) line1 tree) of
        (Nothing, Nothing) -> ([(line2, line1)], [(line1, line2)])
        (Just small, Nothing) -> ([(line2, line1), (line1, small)], [(line1, line2), (line2, small)])
        (Nothing, Just big) -> ([(line2, line1), (big, line2)], [(line1, line2), (big, line1)])
        (Just small, Just big) -> ([(line2, line1), (big, line2), (line1, small)], [(line1, line2), (big, line1), (line2, small)])
    newTree = swapWith (lineOrder x) line1 line2 tree
  in
    (newTree, newNeighbors, newEnemies)

-- Handle updated neighbor information
updateEventQueue :: X -> [(LineSeg, LineSeg)] -> [(LineSeg, LineSeg)] -> EventQueue -> EventQueue
updateEventQueue x new old queue =
  let
    newQueue = foldl (\q (l1, l2) -> addNeighbor x l1 l2 q) queue new
    newerQueue = foldl (\q (l1, l2) -> removeNeighbor x l1 l2 q) newQueue old
  in
    newerQueue

addNeighbor :: X -> LineSeg -> LineSeg -> EventQueue -> EventQueue
addNeighbor x line1 line2 queue =
  case intersect line1 line2 of
    Nothing -> queue
    (Just p@(xInt, _)) -> if x < xInt
                          then Queue.insert (Intersection line1 line2 p) queue
                          else queue

removeNeighbor :: X -> LineSeg -> LineSeg -> EventQueue -> EventQueue
removeNeighbor x line1 line2 queue =
  case intersect line1 line2 of
    Nothing -> queue
    (Just p@(xInt, _)) -> if x < xInt
                          then Queue.delete (Intersection line1 line2 p) queue
                          else queue

findIntersections :: [LineSeg] -> [Point]
findIntersections listOfLines =
  let
    start = (loadEventQueue listOfLines, BinTree.empty, [])
    end = (`fix` start) $ \loop (queue, tree, foundPoints) ->
                             case deleteMin queue of
                               Nothing -> foundPoints
                               Just (removed, newQueue) ->
                                 case removed of
                                   LeftEndPoint l@(LineSeg (x, _) _) ->
                                     let
                                       (newTree, newNeighbors, newEnemies) = insertLine x tree l
                                       newerQueue = updateEventQueue x newNeighbors newEnemies newQueue
                                     in
                                       loop (newerQueue, newTree, foundPoints)
                                   RightEndPoint l@(LineSeg _ (x, _)) ->
                                     let (newTree, newNeighbors, newEnemies) = deleteLine x tree l
                                         newerQueue = updateEventQueue x newNeighbors newEnemies newQueue
                                     in
                                       loop (newerQueue, newTree, foundPoints)
                                   Intersection line1 line2 p@(x, _) ->
                                     let (newTree, newNeighbors, newEnemies) = swapLines x tree line1 line2
                                         newerQueue = updateEventQueue x newNeighbors newEnemies newQueue
                                     in
                                       loop (newerQueue, newTree, p:foundPoints)
  in
    end
