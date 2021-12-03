module Main where

import System.IO ()

groupByTwo :: [Int] -> [(Int, Int)]
groupByTwo x = init $ tail $ zip ([0]++x) (x++[0])

groupByThree :: [Int] -> [(Int, Int, Int)]
groupByThree x = init $ tail $ init $ tail $ zip3 ([0] ++ [0] ++ x) ([0] ++ x ++ [0]) (x ++ [0] ++ [0])

countSecondBigger :: [(Int, Int)] -> Int
countSecondBigger = foldl (\acc curr -> if fst curr < snd curr then acc + 1 else acc) (0::Int)

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let nums = map (\strNum -> read strNum::Int) $ lines inp

  let numsWithNext = groupByTwo nums
  let biggerSecond = countSecondBigger numsWithNext

  let numsGroupedByThree = groupByThree nums
  let sums = map (\(a,b,c) -> a + b + c) numsGroupedByThree

  let sumsWithNext = groupByTwo sums
  let biggerSecondSum = countSecondBigger sumsWithNext

  writeFile "output.txt" $ show biggerSecond ++ " " ++ show biggerSecondSum
  return ()
  