module Main where

safeDivide :: Int -> Int -> Int 
safeDivide x y = round (fromIntegral x / fromIntegral y)

getLowestPointPartOne :: [Int] -> Int
getLowestPointPartOne xs = minimum transformed where
  m = maximum xs
  transformed = map (\x -> foldl (\acc y -> acc + abs (x - y)) 0 xs) [0..m]

getLowestPointPartTwo :: [Int] -> Int
getLowestPointPartTwo xs = minimum transformed where
  m = maximum xs
  transformed = map (\x -> foldl (\acc y -> acc + safeDivide ((1 + abs (y - x)) * abs (y - x)) 2) 0 xs) [0..m]

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let numbers = map (\el -> read el ::Int ) $ words $ map (\el -> if el == ',' then ' ' else el) inp

  writeFile "output.txt" $ show (getLowestPointPartOne numbers) ++ "\n" ++ show (getLowestPointPartTwo numbers)  
  return ()