module Main where

-- If time = t, distance = d, time button is held for = b. Then problem is when:
-- b(t-b) > d.
-- this is just a quadratic in b.
-- 0's are at t/2 +/- sqrt(d - t^2 / 4)
-- just round up the smaller zero and round down the larger zero to find range.

part1 x = do
  let inputs = map (map (read :: String -> Float) . drop 1 . words) x
  let times = head inputs
  let distances = inputs !! 1
  let waysToWin = [findNumberOfWaysToWin t d | x <- zip times distances, let t = fst x, let d = snd x]
  product waysToWin

part2 x = do
  let inputs = map (concat . drop 1 . words) x
  let time = read (head inputs) :: Float
  let distance = read (inputs !! 1) :: Float
  findNumberOfWaysToWin time distance

findNumberOfWaysToWin :: Float -> Float -> Int
findNumberOfWaysToWin t d = 1 + floor upperZero - ceiling lowerZero
  where
    -- cheeky hack to avoid exact ints
    lowerZero = t / 2 - sqrt (t ^ 2 / 4 - d) + 0.001
    upperZero = t / 2 + sqrt (t ^ 2 / 4 - d) - 0.001

main = do
  input <- readFile "day06/input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
