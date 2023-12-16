module Main where

import Data.Data (DataRep (IntRep))
import Data.List (find, groupBy)
import Data.Maybe (isNothing)
import GHC.IO.Handle (NewlineMode (inputNL))

part1 xs = do
  let paras = splitIntoparagraphs xs
      seeds = map (read :: String -> Int) (drop 1 $ words $ head $ head paras)
      maps = map (map (map (read :: String -> Int) . words) . drop 2) (tail paras)
      locations = [foldl applyMap seed maps | seed <- seeds]
  minimum locations

splitIntoparagraphs = groupBy (\x y -> y /= "")

isMapRelevant :: Int -> [Int] -> Bool
isMapRelevant z ms = (ms !! 1 <= z) && (ms !! 1 + ms !! 2 > z)

applyMap :: Int -> [[Int]] -> Int
applyMap x ys = maybe x (findDestination x) relevantMap
  where
    relevantMap = find (isMapRelevant x) ys
    findDestination :: Int -> [Int] -> Int
    findDestination z ms = head ms + z - ms !! 1

part2 xs = do
  let paras = splitIntoparagraphs xs
      seeds = pairUp (map (read :: String -> Int) (drop 1 $ words $ head $ head paras))
      maps = map (map (map (read :: String -> Int) . words) . drop 2) (tail paras)
      locations = foldl findNextLocations seeds maps
  minimum [fst loc | loc <- locations]

pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp [x] = [(x, x)]
pairUp (x : y : xs) = (x, y) : pairUp xs

findIntervals :: [[Int]] -> (Int, Int) -> [(Int, Int)]
findIntervals ys (a, b)
  | isNothing relevantMap, closestIntervalStart > a + b - 1 = [(a, b)]
  | isNothing relevantMap = (a, closestIntervalStart - a) : findIntervals ys (closestIntervalStart, b - closestIntervalStart + a)
  | isIntervalSubsetOfMap = [(a, b)]
  | otherwise = (a, endOfMapRange - a + 1) : findIntervals ys (a + endOfMapRange - a + 1, b - endOfMapRange + a - 1)
  where
    relevantMap = find (isMapRelevant a) ys
    mapRange = maybe 0 (!! 2) relevantMap
    endOfMapRange = maybe 0 (!! 1) relevantMap + mapRange - 1
    endOfInterval = a + b - 1
    isIntervalSubsetOfMap = endOfMapRange >= endOfInterval
    nextMaps = [x | y <- ys, let x = head y, x > a]
    closestIntervalStart =
      if null nextMaps
        then -- ideally I would have infinity here...
          a + b
        else minimum nextMaps

applyMapToTuple :: [[Int]] -> (Int, Int) -> (Int, Int)
applyMapToTuple xs (a, b) = (applyMap a xs, b)

findNextLocations :: [(Int, Int)] -> [[Int]] -> [(Int, Int)]
findNextLocations is ms = map (applyMapToTuple ms) (concatMap (findIntervals ms) is)

main = do
  input <- readFile "day05/input.txt"
  -- print $ part1 $ lines inputN
  print $ part2 $ lines input
