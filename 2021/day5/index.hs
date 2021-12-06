module Main where

import Data.List (partition, find)
import qualified Data.Map as M

lp :: Int -> Int -> Int -> Int -> [(Int, Int)]
lp x1 y1 x2 y2
  | x1 == x2 = if y2 >= y1 then [(x1, y) | y <- [y1..y2]] else [(x1, y) | y <- [y2..y1]]
  | x1 < x2 = [(x, y1) | x <- [x1..x2]]
  | otherwise = [(x, y1) | x <- [x2..x1]]

lp2 :: Int -> Int -> Int -> Int -> [(Int, Int)]
lp2 x1 y1 x2 y2
  | x1 == x2 = if y2 >= y1 then [(x1, y) | y <- [y1..y2]] else [(x1, y) | y <- [y2..y1]]
  | y1 == y2 = if x2 >= x1 then [(x, y1) | x <- [x1..x2]] else [(x, y1) | x <- [x2..x1]]
  | x1 < x2 = if y1 <= y2 then [(x1 + d, y1 + d) | d <- zeroToDiff] else [(x1 + d, y1 - d) | d <- zeroToDiff]
  | x1 > x2 = if y1 <= y2 then [(x1 - d, y1 + d) | d <- zeroToDiff] else [(x1 - d, y1 - d) | d <- zeroToDiff]
  | otherwise = if y1 <= y2 then [(x1 + d, y1 + d) | d <- zeroToDiff] else [(x1 - d, y1 - d) | d <- zeroToDiff]
    where diff = abs (x1 - x2)
          zeroToDiff = [0..diff]

pointToString :: (Int, Int) -> String
pointToString (x, y) = show x ++ "-" ++ show y

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing = False

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line =
  let [x1, y1, x2, y2] = map read $ words $ map (\ch -> if ch `elem` [',', '-', '>'] then ' ' else ch) line
  in ((x1, y1), (x2, y2))

countOccurrences :: [((Int, Int), (Int, Int))] -> M.Map String Int -> M.Map String Int
countOccurrences [] occurrences = occurrences
countOccurrences (xy:xys) occurrences = countOccurrences xys connectedOccurrences where
  ((x1, y1), (x2, y2)) = xy
  linePoints = lp x1 y1 x2 y2
  partitionByExistence p = maybeToBool $ M.lookup (pointToString p) occurrences
  (points, newPoints) = partition partitionByExistence linePoints
  newOccurrences = foldl (\m p -> M.insert (pointToString p) 1 m) M.empty newPoints
  updatedOccurrences = foldl (\m p -> M.insertWith (+) (pointToString p) 1 m) occurrences points
  connectedOccurrences = M.union updatedOccurrences newOccurrences

countOccurancesWithDiagonals :: [((Int, Int), (Int, Int))] -> M.Map String Int -> M.Map String Int
countOccurancesWithDiagonals [] occurrences = occurrences
countOccurancesWithDiagonals (xy:xys) occurrences = countOccurancesWithDiagonals xys connectedOccurrences where
  ((x1, y1), (x2, y2)) = xy
  linePoints = lp2 x1 y1 x2 y2
  partitionByExistence p = maybeToBool $ M.lookup (pointToString p) occurrences
  (points, newPoints) = partition partitionByExistence linePoints
  newOccurrences = foldl (\m p -> M.insert (pointToString p) 1 m) M.empty newPoints
  updatedOccurrences = foldl (\m p -> M.insertWith (+) (pointToString p) 1 m) occurrences points
  connectedOccurrences = M.union updatedOccurrences newOccurrences

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let ls = lines inp
  let parsedLines = map parseLine ls
  let filteredLines = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) parsedLines

  let count = countOccurrences filteredLines M.empty
  let overOrTwo = length $ filter (>= 2) $ M.elems count

  let count2 = countOccurancesWithDiagonals parsedLines M.empty
  let overOrTwo2 = length $ filter (>= 2) $ M.elems count2

  writeFile "output.txt" $ show overOrTwo ++ "\n" ++ show overOrTwo2

  return ()