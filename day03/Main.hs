module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (elemIndices, findIndex, findIndices, nub)

part1 xs = do
  let symbolMap = makeSymbolMatrix xs
      numberIndices = findNumberIndices xs
      validIndices =
        [ [ numIndex | j <- [0 .. (length (numberIndices !! i) - 1)], let numIndex = numberIndices !! i !! j, checkNumberIndicesAreValid numIndex (symbolMap !! i)
          ]
          | i <- [0 .. (length numberIndices - 1)]
        ]
  sum
    [ num | i <- [0 .. (length validIndices - 1)], j <- [0 .. (length (validIndices !! i) - 1)], let numIndex = validIndices !! i !! j, let num = fetchNumber i numIndex xs
    ]

isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol x = (not . isDigit) x

neighbors :: Int -> Int -> [[Bool]] -> [Bool]
neighbors i j matrix = do
  x <- [-1, 0, 1]
  y <- [-1, 0, 1]
  let ni = i + x
      nj = j + y
  -- Check if the indices are valid and return the corresponding value
  [matrix !! ni !! nj | ni >= 0 && nj >= 0 && ni < length matrix && nj < length (head matrix)]

makeAdjSymbolMatrix :: [[Bool]] -> [[Bool]]
makeAdjSymbolMatrix xs =
  [ [or (neighbors i j xs) | j <- [0 .. (length (head xs) - 1)]]
    | i <- [0 .. (length xs - 1)]
  ]

makeSymbolMatrix :: [String] -> [[Bool]]
makeSymbolMatrix = makeAdjSymbolMatrix . map (map isSymbol)

findNumberIndices = map (groupConsecutive . findIndices isDigit)

groupConsecutive :: [Int] -> [[Int]]
groupConsecutive [] = []
groupConsecutive xs = reverse $ go [head xs] (tail xs) []
  where
    go current [] acc = reverse current : acc
    go current (y : ys) acc
      | y == head current + 1 = go (y : current) ys acc
      | otherwise = go [y] ys (reverse current : acc)

checkNumberIndicesAreValid :: [Int] -> [Bool] -> Bool
checkNumberIndicesAreValid xs ys =
  foldr (\x -> (||) (ys !! x)) False xs

fetchNumber :: Int -> [Int] -> [String] -> Int
fetchNumber x [] zs = 0
fetchNumber x (y : ys) zs = (digitToInt (zs !! x !! y) :: Int) * 10 ^ pow + fetchNumber x ys zs
  where
    pow = length ys

part2 xs = do
  let symbolMap = map (map (== '*')) xs
      numberMap = map (map isDigit) xs
      isValidStar i j = do
        let neighbors = neighborMatrix i j numberMap
        countIslands neighbors == 2
      validStars =
        [ [ sym | j <- [0 .. (length (symbolMap !! i) - 1)], let isStar = symbolMap !! i !! j, let sym = isStar && isValidStar i j
          ]
          | i <- [0 .. (length symbolMap - 1)]
        ]
      gearRatios =
        [ fetchGearRatio i j numberMap xs | i <- [0 .. (length validStars - 1)], j <- [0 .. length (head validStars) - 1], validStars !! i !! j
        ]
  sum gearRatios

neighborMatrix :: Int -> Int -> [[a]] -> [[a]]
neighborMatrix i j matrix = [[matrix !! x !! y | y <- map (+ j) [-1, 0, 1], y >= 0, y < length (head matrix)] | x <- map (+ i) [-1, 0, 1], x >= 0, x < length matrix]

countIslands :: [[Bool]] -> Int
countIslands [] = 0
countIslands (x : xs)
  | x == [True, False, True] = 2 + countIslands xs
  | x == [False, False, False] = 0 + countIslands xs
  | otherwise = 1 + countIslands xs

fetchGearRatio :: Int -> Int -> [[Bool]] -> [String] -> Int
fetchGearRatio i j bs cs = do
  let neighbors = neighborMatrix i j bs
      indices = map (elemIndices True) neighbors
      numbers = [fetchNumber2 (i + x - 1) (y + j) cs | x <- [0, 1, 2], y <- indices !! x]
  -- could break if two numbers are the same next to each other...
  product $ nub numbers

fetchNumber2 :: Int -> Int -> [String] -> Int
fetchNumber2 i j xs = do
  let row = xs !! i
      prevNumbers = take j row
      nextNumbers = drop j row
      number = (reverse . takeWhile isDigit . reverse) prevNumbers ++ takeWhile isDigit nextNumbers
  read number :: Int

main = do
  input <- readFile "day03/input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
