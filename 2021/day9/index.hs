module Main where
import Data.Function (on)
import Data.List (sortBy, nubBy)
import qualified Data.Map as M

type Point = (Int, Int)

getNeighbors :: [[(Point, Char)]] -> (Int, Int) -> String
getNeighbors grid (x, y) = map (\(x', y') -> snd ((grid !! x') !! y')) $ filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < length grid && y' < length (head grid)) $
  [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]

partOne :: [[(Point, Char)]] -> Int
partOne indexedMatrix = sum riskLevels where
  riskLevels = map (\x -> (read [x] :: Int) + 1) foundLowPoints
  foundLowPoints  = foldl foldMatrix [] indexedMatrix
  foldMatrix acc row = acc ++ foldl foldRow [] row
  foldRow acc (point, char) = if all (> char) (getNeighbors indexedMatrix point) then acc ++ [char] else acc

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing = False

partTwo :: [[(Point, Char)]] -> Int
partTwo indexedMatrix = product (take 3 (sortBy (compare `on` length) basins)) where
  basins = getBasins foundLowPoints indexedMatrix []
  getBasins lowPoints matrix = map processLowPoint matrix [] lowPoints
  processLowPoints matrix processedPoints (point, char) = if all (\(_, c) -> c == '9') (nubBy (\(p1, c1) (p2, c2) -> fst p1 == fst p2 && snd p1 == snd p2) $ foldMap (getNeighbors matrix) processedPoints) then length processedPoints else processLowPoints matrix processedPoints (point, char) 
  foundLowPoints  = foldl foldMatrix [] indexedMatrix
  foldMatrix acc row = acc ++ foldl foldRow [] row
  foldRow acc (point, char) = if all (> char) (getNeighbors indexedMatrix point) then acc ++ [(point, char)] else acc

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let indexedRows = zip [0..] $ lines inp
  let indexedMatrix = map (\(x, line) -> zipWith (\ y char -> ((x, y), char)) [0..] line) indexedRows

  print (partOne indexedMatrix)

  return ()