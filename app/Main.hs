module Main where

import LineSegment

getLineSeg :: String -> LineSeg
getLineSeg = read


main :: IO ()
main = do
  content <- readFile "input/line-segments"
  putStrLn $ show $ map getLineSeg (lines content)
