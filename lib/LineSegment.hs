module LineSegment ( Point ( Pt )
                   , LineSeg ( Line ) ) where

data Point = Pt Int Int
             deriving (Eq, Ord, Read, Show)

data LineSeg = Line
               Point
               Point
             deriving (Eq, Ord, Read, Show)
