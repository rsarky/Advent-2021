import System.IO

data Command = Command { direction :: String
                       , arg :: Int
                       } deriving (Show)

data AimState = State { aim :: Int,
                        depth :: Int,
                        hPos :: Int
                      }

isDepthCommand :: Command -> Bool
isDepthCommand (Command d _) = d == "up" || d == "down"

isForwardCommand :: Command -> Bool
isForwardCommand (Command d _) = d == "forward"

calculateDepth :: [Command] -> Int
calculateDepth commands = sum depths
  where 
    depths = map getNumArg depthCommands
    depthCommands = filter isDepthCommand commands
    getNumArg (Command d a) = if d == "up" then -a else a

calculatePos :: [Command] -> Int
calculatePos commands = sum args
  where
    args = map arg forwardCommands
    forwardCommands = filter isForwardCommand commands

calculateState :: AimState -> Command -> AimState
calculateState (State a d h) (Command dir x)
  | dir == "down" = State (a+x) d h
  | dir == "up" = State (a-x) d h
  | otherwise = State a (d + a*x) (h+x)

calculatePositions :: [Command] -> AimState
calculatePositions commands = foldl calculateState (State 0 0 0) commands

calculateDepthWithAim :: [Command] -> Int
calculateDepthWithAim commands = depth $ calculatePositions commands

calculatePosWithAim :: [Command] -> Int
calculatePosWithAim commands = hPos $ calculatePositions commands

main = do
  input <- readFile "input.txt"
  let commands = parse $ lines input
      depth = calculateDepth commands
      pos = calculatePos commands
      part1 = depth*pos
      depthWithAim = calculateDepthWithAim commands
      posWithAim = calculatePosWithAim commands
      part2 = depthWithAim * posWithAim
  print part1
  print part2

parse :: [String] -> [Command]
parse = map parseLine


parseLine :: String -> Command
parseLine line = Command direction argAsInt
  where direction = head $ words line
        argAsInt = (read $ last $ words line) :: Int
