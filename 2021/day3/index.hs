module Main where

import Data.List (transpose, isPrefixOf, find)
import Data.Maybe (fromMaybe)

bintodec :: [Bool] -> Int
bintodec x = foldr (\x y -> fromEnum x + 2*y) 0 (reverse x)

countOnesAndZeros :: Int -> Char -> Int
countOnesAndZeros sum digit = if digit == '1' then sum + 1 else sum - 1

boolToChar :: Bool -> Char
boolToChar True = '1'
boolToChar False = '0'

charToBool :: Char -> Bool
charToBool '1' = True
charToBool '0' = False
charToBool _ = error "Invalid character"

getBinaryGammaPartOne :: [String] -> Int
getBinaryGammaPartOne xs = bintodec rawBooleanGamma where
    transposedNumbers = transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanGamma = map (> 0) sums

getBinaryGammaPartTwo :: [String] -> Int -> String
getBinaryGammaPartTwo xs depth = rawBinaryGamma where
    transposedNumbers = take depth $ transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanGamma = map (>= 0) sums
    rawBinaryGamma = map boolToChar rawBooleanGamma

getBinaryEpsilonPartOne :: [String] -> Int
getBinaryEpsilonPartOne xs = bintodec rawBooleanEpsilon where
    transposedNumbers = transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanEpsilon = map (< 0) sums

getBinaryEpsilonPartTwo :: [String] -> Int -> String
getBinaryEpsilonPartTwo xs depth = rawBinaryEpsilon where
    transposedNumbers = take depth $ transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanEpsilon = map (<= 0) sums
    rawBinaryEpsilon = map boolToChar rawBooleanEpsilon

filterToOneNumberGamma :: [String] -> Int -> String
filterToOneNumberGamma [one] _ = one
filterToOneNumberGamma xs depth = filterToOneNumberGamma filtered (depth + 1) where
    nextBinaryGamma = getBinaryGammaPartTwo xs depth
    filtered = filter (isPrefixOf nextBinaryGamma) xs

filterToOneNumberEpsilon :: [String] -> Int -> String
filterToOneNumberEpsilon [one] _ = one
filterToOneNumberEpsilon xs depth = filterToOneNumberEpsilon filtered (depth + 1) where
    nextBinaryEpsilon = getBinaryEpsilonPartTwo xs depth
    filtered = filter (isPrefixOf nextBinaryEpsilon) xs

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let numbers = lines inp

  let gamma = getBinaryGammaPartOne numbers
  let epsilon = getBinaryEpsilonPartOne numbers

  let gammaRating = bintodec $ map charToBool $ filterToOneNumberGamma numbers 1
  let epsilonRating = bintodec $ map charToBool $ filterToOneNumberEpsilon numbers 1

  -- for unknown reasons the epsilonRating is not working and yields infinite loop 
  writeFile "output.txt" $ show gamma ++ " * " ++ show epsilon ++ " = " ++ show (gamma * epsilon) ++ "\n" ++ show gammaRating ++ " * " -- ++ show epsilonRating ++ " = " ++ show (gammaRating * epsilonRating)

  return ()