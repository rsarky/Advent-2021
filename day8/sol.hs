import System.IO
import Text.Regex.Posix

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  print parsed

parseInput :: String -> [([String], [String])]
parseInput s = map parseLine $ lines s

parseLine :: String -> ([String], [String])
parseLine l = (words l1, words l2)
  where (l1:l2:[]) = tail.head $ (l =~ "(.*) \\| (.*)" :: [[String]])

