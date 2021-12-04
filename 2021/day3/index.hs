module Main where

import Data.List (transpose, isPrefixOf, find)
import Data.Maybe (fromMaybe)

bintodec :: [Bool] -> Int
bintodec x = foldr (\x y -> fromEnum x + 2*y) 0 (reverse x)

countOnesAndZeros :: Int -> Char -> Int
countOnesAndZeros sum digit = if digit == '1' then sum + 1 else sum - 1

boolToIntChar :: Bool -> Char
boolToIntChar True = '1'
boolToIntChar False = '0'

charToBool :: Char -> Bool
charToBool '1' = True
charToBool '0' = False
charToBool _ = error "Invalid character"

getBinaryGammaPartOne :: [String] -> Int
getBinaryGammaPartOne xs = bintodec rawBooleanGamma where
    transposedNumbers = transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanGamma = map (> 0) sums

getBinaryGammaPartTwo :: [String] -> String
getBinaryGammaPartTwo xs = rawBinaryGamma where
    transposedNumbers = transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanGamma = map (> 0) sums
    rawBinaryGamma = map boolToIntChar rawBooleanGamma

getBinaryEpsilonPartOne :: [String] -> Int
getBinaryEpsilonPartOne xs = bintodec rawBooleanEpsilon where
    transposedNumbers = transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanEpsilon = map (< 0) sums

getBinaryEpsilonPartTwo :: [String] -> String
getBinaryEpsilonPartTwo xs = rawBinaryEpsilon where
    transposedNumbers = transpose xs
    sums = map (foldl countOnesAndZeros 0) transposedNumbers
    rawBooleanEpsilon = map (< 0) sums
    rawBinaryEpsilon = map boolToIntChar rawBooleanEpsilon

filterToOneNumberGamma :: [String] -> Int -> String
filterToOneNumberGamma [one] _ = one
filterToOneNumberGamma xs depth = filterToOneNumberGamma filtered (depth + 1) where
    nextBinaryGamma = getBinaryGammaPartTwo xs
    filtered = filter (\num -> take depth nextBinaryGamma `isPrefixOf` num) xs

filterToOneNumberEpsilon :: [String] -> Int -> String
filterToOneNumberEpsilon [one] _ = one
filterToOneNumberEpsilon xs depth = filterToOneNumberEpsilon filtered (depth + 1) where
    nextBinaryEpsilon = getBinaryEpsilonPartTwo xs
    filtered = filter (\num -> take depth nextBinaryEpsilon `isPrefixOf` num) xs

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