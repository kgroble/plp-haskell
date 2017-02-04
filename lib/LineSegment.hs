module LineSegment ( Point ( Pt )
                   , LineSeg ( Line ) ) where

data Point = Pt Int Int
             deriving (Eq, Ord)

data LineSeg = Line
               Point
               Point
             deriving (Eq, Ord)
