module BentleyOttmann ( findIntersections
                      , LineSeg
                      , constructLineSeg ) where

import Data.Function (fix)
import BinTree

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
constructLineSeg p1 p2
  | p1 <= p2 = LineSeg p1 p2
  | otherwise = LineSeg p2 p1


extractX :: XEvent -> X
extractX (LeftEndPoint (LineSeg (x, _) _)) = x
extractX (RightEndPoint (LineSeg _ (x, _))) = x
extractX (Intersection _ _ (x, _)) = x

instance Ord XEvent where
  event1 < event2 = extractX event1 < extractX event2
  event1 <= event2 = event1 < event2 || event1 == event2

-- instance Ord LineSeg where
--   (LineSeg (ax1, _) (_, _)) < l2 =
--     case intersectX l2 ax1 of
--       Nothing -> False
--       Just (x, _) -> ax1 < x
--   l1 <= l2 = (l1 == l2) || l1 < l2

type EventQueue = BinTree XEvent
type LineTree = BinTree LineSeg

loadEventQueue :: [LineSeg] -> EventQueue
loadEventQueue listOfLines = foldl (flip insert) empty (map LeftEndPoint listOfLines)

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

intersectX :: LineSeg -> X -> Maybe Point
intersectX (LineSeg (x1, y1) (x2, y2)) x
  | x < x1 || x2 < x = Nothing
  | otherwise =
    let y = y1 + ((y2 - y1) / (x2 - x1)) * (x - x1)
    in Just (x, y)

lineOrder :: X -> LineSeg -> Double
lineOrder x line =
  case intersectX line x of
    (Just (_, b)) -> b
    Nothing -> undefined

-- findIntersections [LineSeg (-1, -1) (1, 1), LineSeg (-1, 1) (1, -1)]

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

-- neighbors :: LineSeg -> LineTree -> (Maybe LineSeg, Maybe LineSeg)
-- neighbors line tree =
--   listNeighbors line (toList tree)

-- listNeighbors :: LineSeg -> [LineSeg] -> (Maybe LineSeg, Maybe LineSeg)
-- listNeighbors _ [] = (Nothing, Nothing)
-- listNeighbors _ [_] = (Nothing, Nothing)
-- listNeighbors line [first, second]
--   | line == first = (Nothing, Just second)
--   | line == second = (Just first, Nothing)
--   | otherwise = (Nothing, Nothing)
-- listNeighbors line (first:second:third:rest)
--   | line == first = (Nothing, Just second)
--   | line == second = (Just first, Just third)
--   | otherwise = listNeighbors line (second:third:rest)

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
                          then insert (Intersection line1 line2 p) queue
                          else queue

removeNeighbor :: X -> LineSeg -> LineSeg -> EventQueue -> EventQueue
removeNeighbor x line1 line2 queue =
  case intersect line1 line2 of
    Nothing -> queue
    (Just p@(xInt, _)) -> if x < xInt
                          then delete (Intersection line1 line2 p) queue
                          else queue

findIntersections :: [LineSeg] -> [Point]
findIntersections listOfLines =
  let
    start = (loadEventQueue listOfLines, empty, [])
    end = (`fix` start) $ \loop (queue, tree, foundPoints) ->
                             case viewMin queue of
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
