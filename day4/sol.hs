import System.IO
import Data.List
import Data.List.Split
import Debug.Trace

main = do
  -- readFile leads a file lazily. ie Doesnt put everthing into memory
  input <- readFile "input.txt"
  let (nums, boards) = parseInput $ lines input
      allStates = findAllStatesFlattened nums boards
      firstWinner = findFirstWinningBoard allStates
      lastWinner = findLastWinningBoard nums boards
      part1 = scoreWinner nums firstWinner
      part2 = scoreWinner nums lastWinner

  print part1
  print part2

data Elem = Elem {num :: Int,
                  mark :: Bool} deriving Show

type Board = [[Elem]]

parseInput :: [String] -> ([Int], [Board])
parseInput input = parseLines $ splitAtEmptyLines input
  where splitAtEmptyLines = divide ""
        parseLines (nums:boards) = (parseNums (head nums), map (formBoard.parseBoard) boards)
        parseNums xs = map read (splitOn "," xs) :: [Int]
        parseBoard = map parseSpaceSepNums
        parseSpaceSepNums nums = map read (words nums)

formBoard :: [[Int]] -> Board
formBoard intBoard = map listToElem intBoard
  where listToElem = map intToElem
        intToElem x = Elem x False

markBoard :: Int -> Board -> Board
markBoard x b = map (markInList x) b

-- This changes one list to another. Contrast to how I would modify the list in place
-- if doing this in an imperative way
markInList :: Int -> [Elem] -> [Elem]
markInList item [] = []
markInList item (x:xs)
  | num x == item = (Elem (num x) True) : markInList item xs
  | otherwise = x : markInList item xs

-- TODO: this can perhaps be writen in a better way?
divide :: String -> [String] -> [[String]]
divide x [] = []
divide x arr =
  let (match, rest) = span (/=x) arr
      getRest [] = []
      getRest rest = tail rest
  in match : (divide x $ getRest rest)

checkWin :: Board -> Bool
checkWin board = anyRowMarked || anyColMarked
  where anyRowMarked = or rowStatuses 
        anyColMarked = or colStatuses
        rowStatuses = map findRowStatus board
        colStatuses = map findRowStatus (transpose board)
        findRowStatus row = and (map mark row)

findBoardStates :: [Int] -> Board -> [Board]
findBoardStates nums board = scanl (flip markBoard) board nums

-- Returns states of each board after applying each number chronologically
findAllStatesFlattened :: [Int] -> [Board] -> [Board]
findAllStatesFlattened nums boards = concat $ transpose boardStates
  where boardStates = map (findBoardStates nums) boards

findAllBoardStates :: [Int] -> [Board] -> [[Board]]
findAllBoardStates nums boards = map (findBoardStates nums) boards

findFirstWinningBoard :: [Board] -> Board
findFirstWinningBoard [] = error "no winning board"
findFirstWinningBoard (x:xs)
  | checkWin x = x
  | otherwise = findFirstWinningBoard xs

-- todo: better way
-- Current approach: Find the last winning board by finding the first losing board in the
-- reverse order of all states. I want essentially the next state of this board.
-- To do this I go over all states again and find the winning board state which matches
-- the losing boadrd found earlier
findLastWinningBoard :: [Int] -> [Board] -> Board
findLastWinningBoard nums boards = findWinningState winningBoard allStates
  where allStates = findAllStatesFlattened nums boards
        winningBoard = findFirstLosingBoard $ reverse allStates
        findFirstLosingBoard [] = error "no losing board"
        findFirstLosingBoard (x:xs)
          | not (checkWin x) = x
          | otherwise = findFirstLosingBoard xs
        findWinningState board (x:xs)
          | compareBoard board x && checkWin x = x
          | otherwise = findWinningState board xs

compareBoard :: Board -> Board -> Bool
compareBoard b1 b2 = and $ map compareRows (zip b1 b2)
  where compareRows (r1,r2) = (map num r1) == (map num r2)
    

-- the way I am finding the winning number is def. hacky. I should have found it when
-- I was finding the winning board
scoreWinner :: [Int] -> Board -> Int
scoreWinner nums board = unmarkedSum * winningNum
  where unmarkedSum = sum $ map num $ filter unmarked $ concat board
        unmarked = not.mark
        winningNum = findWinningNum nums $ reset board

findWinningNum :: [Int] -> Board -> Int
findWinningNum [] board = error "no winning num"
findWinningNum (x:xs) board
  | checkWin newBoard = x
  | otherwise = findWinningNum xs newBoard
  where newBoard = markBoard x board

-- reset marks
reset :: Board -> Board
reset board = map resetRow board
  where resetRow = map resetElem
        resetElem x = Elem (num x) False
