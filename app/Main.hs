module Main where

import BentleyOttmann
import System.Environment
import Data.String.Utils

getLineSeg :: String -> LineSeg
getLineSeg str = lineSegFromPoint (read str::((Double, Double), (Double, Double)))

lineSegFromPoint :: ((Double, Double), (Double, Double)) -> LineSeg
lineSegFromPoint (p1@(_, _), p2@(_, _)) = constructLineSeg p1 p2

formatOut :: String -> String
formatOut = replace "),(" ")\n(" . replace "[" "" . replace "]" ""

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inpath, outpath] -> do
      content <- readFile inpath
      writeFile outpath $ formatOut $ show $ findIntersections $ foldl (flip (:)) [] $ map getLineSeg (lines content)
    _ ->
      putStrLn "Arguments must be of the form './prog <inpath> <outpath>'"
