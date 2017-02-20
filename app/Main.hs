module Main where

import BentleyOttmann

getLineSeg :: String -> LineSeg
getLineSeg = read


main :: IO ()
main = do
  content <- readFile "input/line-segments"
  putStrLn $ show $ findIntersections $ foldl (flip (:)) [] $ map getLineSeg (lines content)
