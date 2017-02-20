module Main where

import BentleyOttmann
import System.Environment

getLineSeg :: String -> LineSeg
getLineSeg = read



main :: IO ()
main = do
  args <- getArgs
  case args of
    [inpath, outpath] -> do
      content <- readFile inpath
      writeFile outpath $ show $ findIntersections $ foldl (flip (:)) [] $ map getLineSeg (lines content)
    _ ->
      putStrLn "Arguments must be of the form './prog <inpath> <outpath>'"
