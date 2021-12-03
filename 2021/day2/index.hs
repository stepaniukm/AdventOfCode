module Main where

parseCommands :: [String] -> (Int, Int)
parseCommands = foldl (\acc fullCommand -> parseCommand acc (command fullCommand) (amount fullCommand)) (0, 0) where
  command f = head $ words f
  amount f = read $ last $ words f :: Int
  parseCommand acc command amount = case command of 
    "forward" -> (fst acc, snd acc + amount)
    "down" -> (fst acc + amount, snd acc)
    "up" -> (fst acc - amount, snd acc)
    _ -> acc

parseCommandsPartTwo :: [String] -> (Int, Int, Int)
parseCommandsPartTwo = foldl (\acc fullCommand -> parseCommand acc (command fullCommand) (amount fullCommand)) (0, 0, 0) where
  command f = head $ words f
  amount f = read $ last $ words f :: Int
  parseCommand (depth, horizontal, aim) command amount = case command of 
    "forward" -> (depth + (aim * amount), horizontal + amount, aim)
    "down" -> (depth, horizontal, aim + amount)
    "up" -> (depth, horizontal, aim - amount)
    _ -> (depth, horizontal, aim)

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let commands = lines inp
  let (depth, horizontal) = parseCommands commands
  let (depth2, horizontal2, aim2) = parseCommandsPartTwo commands
  
  writeFile "output.txt" $ show (depth * horizontal) ++ " " ++ show (depth2 * horizontal2)

  return ()