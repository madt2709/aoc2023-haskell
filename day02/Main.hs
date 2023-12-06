module Main where

import Data.List
import Data.List.Split

part1 = foldl' (+) 0 . map (+ 1) . findIndices (all checkLimit) . map (map countCubes) . parseIntoListOfSets

parseIntoListOfSets = map (drop2OnlyFirstElement . map words . splitOn ";" . filter (`notElem` ","))

drop2OnlyFirstElement :: [[String]] -> [[String]]
drop2OnlyFirstElement (x : xs) = drop 2 x : xs

countCubes :: [String] -> [Int]
countCubes [] = [0, 0, 0]
countCubes (n : s : xs)
  | s == "red" = zipWith (+) [read n :: Int, 0, 0] (countCubes xs)
  | s == "green" = zipWith (+) [0, read n :: Int, 0] (countCubes xs)
  | s == "blue" = zipWith (+) [0, 0, read n :: Int] (countCubes xs)
  | otherwise = countCubes xs

checkLimit :: [Int] -> Bool
checkLimit xs = all (>= 0) (zipWith (-) [12, 13, 14] xs)

part2 = foldl' (+) 0 . map (product . maxList . map countCubes) . parseIntoListOfSets

maxList :: [[Int]] -> [Int]
maxList [] = []
maxList (x : xs) = if null xs then x else zipWith max x (maxList xs)

main = do
  input <- readFile "day02/input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
