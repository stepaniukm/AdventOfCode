module Main where

import Data.List (transpose)

parseBoardLines :: [String] -> [[[Int]]] -> [[[Int]]]
parseBoardLines [] boards = boards
parseBoardLines lines boards = parseBoardLines newLines newBoards where
  newLines = drop 6 lines
  parseBoardLinesInside lines = map (map (\num -> read num :: Int) . words) lines
  parsedBoard = parseBoardLinesInside (take 5 lines)
  newBoards = parsedBoard : boards

getBingoWinnerPartOne :: [[[Int]]] -> [Int] -> ([[Int]], Int)
getBingoWinnerPartOne [winner] [winnerNumber] = (winner, winnerNumber)
getBingoWinnerPartOne boards bingoInput = getBingoWinnerPartOne newBoards newBingoInput where
  boardsWithReplacedNumbers = map (map (map (\el -> if el == head bingoInput then -1 else el))) boards
  isWinner board = any (all (== -1)) board || any (all (== -1)) (transpose board)
  isThereWinner = any isWinner boardsWithReplacedNumbers
  newBoards = filter (\board -> not isThereWinner || isWinner board) boardsWithReplacedNumbers
  newBingoInput = if isThereWinner then take 1 bingoInput else drop 1 bingoInput

getBingoWinnerPartTwo :: [[[Int]]] -> [Int] -> [[[Int]]] -> ([[Int]], Int)
getBingoWinnerPartTwo [] [winnerNumber] winners = (last winners, winnerNumber)
getBingoWinnerPartTwo boards bingoInput winners = getBingoWinnerPartTwo newBoards newBingoInput newWinners where
  boardsWithReplacedNumbers = map (map (map (\el -> if el == head bingoInput then -1 else el))) boards
  isWinner board = any (all (== -1)) board || any (all (== -1)) (transpose board)
  newBoards = filter (not . isWinner) boardsWithReplacedNumbers
  newWinners = winners ++ filter isWinner boardsWithReplacedNumbers
  newBingoInput = if null newBoards then take 1 bingoInput else drop 1 bingoInput

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let l = lines inp
  let (bingoInput, boardLines) = (head l, drop 2 l)
  let bingoInputArray = map (\ch -> read ch::Int) $ words $ map (\ch -> if ch == ',' then ' ' else ch) bingoInput
  let boards = parseBoardLines boardLines []

  let (winnerBoard, winnerNumber) = getBingoWinnerPartOne boards bingoInputArray
  let (winnerBoard2, winnerNumber2) = getBingoWinnerPartTwo boards bingoInputArray []

  let sumOfWinnerBoard = sum $ filter (\el -> el /= -1) $ concat winnerBoard
  let sumOfWinnerBoard2 = sum $ filter (\el -> el /= -1) $ concat winnerBoard2

  writeFile "output.txt" $ show (winnerNumber *  sumOfWinnerBoard) ++ "\n" ++ show (winnerNumber2 * sumOfWinnerBoard2)
  return ()
