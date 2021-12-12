import System.IO
import Data.List
import Lib

findMedian :: [Int] -> Int
findMedian nums = sort nums !! midIndex
  where midIndex = div (length nums) 2

findMean :: [Int] -> Int
findMean nums = round $ a / b
 where a = fromIntegral $ sum nums
       b = fromIntegral $ length nums

findDistances :: [Int] -> Int -> Int
findDistances nums pos = 
  foldl (\acc x -> acc + abs (x - pos)) 0 nums

-- Fuel cost when cost increases quadratically with distance
findFuelCost :: [Int] -> Int -> Int
findFuelCost nums pos =
  foldl (\acc x -> acc + dist x pos) 0 nums
    where dist a b = let c = abs (a-b) in div (c * (c+1)) 2

-- Least possible fuel cost, where fuel cost increases quad. with distance
-- Brute Force
findLeastFuelCost :: [Int] -> Int
findLeastFuelCost nums = minimum $ map (findFuelCost nums) [minimum nums..maximum nums]



main = do
  input <- readFile "input.txt"
  let nums = readCommaSeparatedInts input
      -- The median is the minimizer of the sum of absolute distances of a set of numbers
      pos1 = findMedian nums
      pos2 = findMean nums
      part1 = findDistances nums pos1
      part2 = findLeastFuelCost nums
  print part1
  print part2
