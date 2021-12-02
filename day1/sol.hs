import System.IO
import Debug.Trace

main = do
  input <- readFile "input.txt"
  -- What does let do in Haskell?
  let nums = f $ lines input
      part1 = numPositiveDeltas nums
      part2 = numPositiveDeltas $ slidingWindowSums
      slidingWindowSums = map sum (makeSlidingWindow 3 nums)
  print part1
  print part2

-- Difference between Integer and Int
f :: [String] -> [Int]
f = map read

numPositiveDeltas :: [Int] -> Int
numPositiveDeltas nums = length posDeltas
  where posDeltas = filter (>0) deltas
        deltas = map (\x -> (fst x) - (snd x)) combos
        combos = zip (tail nums) nums

makeSlidingWindow :: Int -> [Int] -> [[Int]]
makeSlidingWindow windowSize nums = 
  if ((length nums) == windowSize)
    then [nums]
    else (take windowSize nums) : (makeSlidingWindow windowSize (drop 1 nums))
