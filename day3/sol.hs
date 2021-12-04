import System.IO
import Data.List

mostCommon :: [Char] -> Char
mostCommon s = if ones >= zeroes then '1' else '0'
  where zeroes = length s - ones
        ones = sum $ toIntArr s

leastCommon :: [Char] -> Char
leastCommon s = if mostCommon s == '1' then '0' else '1'

findX :: [[Char]] -> ([Char] -> Char) -> Int
findX nums fn = binToInt bin
  where bin = map fn $ transpose nums

findGamma :: [[Char]] -> Int
findGamma nums = findX nums mostCommon

findEpsilon :: [[Char]] -> Int
findEpsilon nums = findX nums leastCommon

binToInt :: [Char] -> Int
binToInt num = fst $ foldr adder (0, 0) intArr
  where adder curr (res, i) = (res + curr * 2^i, i+1)
        intArr = toIntArr num

toIntArr :: [Char] -> [Int]
toIntArr num = map (read . (:"")) num :: [Int]

findRating :: [[Char]] -> Int -> ([Char] -> Char) -> Int
findRating [num] x fn = binToInt num
findRating nums x fn = findRating (filter (checkBitAtPos) nums) (x+1) fn
  where checkBitAtPos n = bitAtPos == (n !! x)
        bitAtPos = (map fn $ transpose nums) !! x

findO2Rating nums = findRating nums 0 mostCommon
findCO2Rating nums = findRating nums 0 leastCommon

main = do
  input <- readFile "input.txt"
  let nums = lines input
      gamma = findGamma nums
      epsilon = findEpsilon nums
      part1 = gamma*epsilon
      o2 = findO2Rating nums
      co2 = findCO2Rating nums
      part2 = o2*co2
  print part1
  print part2

