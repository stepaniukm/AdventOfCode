module Main where

import Data.Maybe (fromMaybe)
import Data.List ((\\), nub, sort, partition)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Text.XHtml (input)

partOne :: [([String], [String])] -> Int
partOne ls = sumOfLines where
  sumIfSimple = foldl (\acc curr -> if length curr `elem` [2,3,4,7] then acc + 1 else acc) 0
  sumOfLines = sum $ map (\(_, b) -> sumIfSimple b) ls

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing = False

addSectors :: String -> String -> String
addSectors a b = nub $ a ++ b

diffSectors :: String -> String -> String
diffSectors a b = a \\ b

deduceCorrectSignalsFromSignals :: [String] -> M.Map String Int
deduceCorrectSignalsFromSignals xs = correctMap where
  (oneL, r) = partition (\x -> length x == 2) xs
  (fourL, r2) = partition (\x -> length x == 4) r
  (sevenL, r3) = partition (\x -> length x == 3) r2
  (eightL, r4) = partition (\x -> length x == 7) r3
  (one, four, seven, eight) = (head oneL, head fourL, head sevenL, head eightL)
  sectorA = diffSectors seven one
  sectorsBD = diffSectors four one
  sectorsEG = diffSectors eight $ addSectors four sectorA
  (sixL, r5) = partition (\x -> length (addSectors x one) == 7 && x /= eight) r4
  (nineL, r6) = partition (\x -> length (addSectors x sectorsEG) == 7 && x /= eight) r5
  (zeroL, r7) = partition (\x -> length (addSectors x sectorsBD) == 7 && x /= eight) r6
  (threeL, r8) = partition (\x -> length (diffSectors x one) == 3 && x /= seven) r7
  (six, nine, zero, three) = (head sixL, head nineL, head zeroL, head threeL)
  (fiveL, r9) = partition (\x -> length (diffSectors six x) == 1 && x /= eight) r8
  five = head fiveL
  two = head r9
  correctMap = M.fromList [(sort zero, 0), (sort one, 1), (sort two, 2), (sort three, 3), (sort four, 4), (sort five, 5), (sort six, 6), (sort seven, 7), (sort eight, 8), (sort nine, 9)]

solveLine :: ([String],[String]) -> Int
solveLine (messedUpSignals, inputSignals) = decodedSignals where
  messedUpSignalToSignal = deduceCorrectSignalsFromSignals messedUpSignals
  getCorrectSignal signal = fromMaybe 0 $ M.lookup (sort signal) messedUpSignalToSignal
  decodedSignals = read $ foldMap (show . getCorrectSignal) inputSignals

partTwo :: [([String], [String])] -> Int
partTwo ls = sum solutions where
  solutions = map solveLine ls

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let ls = lines inp
  let parsedLines = map (splitAt 10 . (words . map (\el -> if el == '|' then ' ' else el))) ls

  writeFile "output.txt" $ show (partOne parsedLines) ++ "\n" ++ show (partTwo parsedLines)

  return ()