module Main where

import Data.List (elemIndex, findIndex)
import Data.Maybe (isNothing)
import System.Posix (BaudRate (B0))

part1 = sum . map (power2Positives . length . intersection) . splitNumbers

splitNumbers = map (splitAtLine . drop 2 . words)

splitAtLine :: [String] -> [[String]]
splitAtLine xs = do
  let index = elemIndex "|" xs
      helper x = [take x xs, drop (x + 1) xs]
  maybe [[]] helper index

intersection :: [[String]] -> [String]
intersection xs = filter (`elem` head xs) (last xs)

power2Positives :: Int -> Int
power2Positives x
  | x < 1 = 0
  | otherwise = 2 ^ (x - 1)

part2 = countScratchCards . map (length . intersection) . splitNumbers

countScratchCards :: [Int] -> Int
countScratchCards xs = sum [numberOfCopiesOfCardN i xs | i <- [0 .. (length xs - 1)]]

numberOfCopiesOfCardN :: Int -> [Int] -> Int
numberOfCopiesOfCardN 0 xs = 1
numberOfCopiesOfCardN n xs = 1 + sum [numberOfCopiesOfCardN i xs | i <- [0 .. n - 1], n - i <= xs !! i]

main = do
  input <- readFile "day04/input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
