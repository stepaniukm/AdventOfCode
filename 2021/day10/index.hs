module Main where
import Data.Maybe (isJust, isNothing)
import Data.List (sort)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

getBadBracket :: String -> Maybe Char -> [Char] -> Maybe Char
getBadBracket _ (Just a) _ = Just a
getBadBracket _ Nothing [] = Nothing
getBadBracket stack Nothing (c:cs)
  | isOpenedBracket c = getBadBracket (c:stack) Nothing cs
  | safeHead stack == Just (getMatchingBracket c) = getBadBracket (tail stack) Nothing cs
  | otherwise = getBadBracket [] (Just c) []

getMatchingBracket :: Char -> Char
getMatchingBracket '{' = '}'
getMatchingBracket '(' = ')'
getMatchingBracket '[' = ']'
getMatchingBracket '<' = '>'
getMatchingBracket '}' = '{'
getMatchingBracket ')' = '('
getMatchingBracket ']' = '['
getMatchingBracket '>' = '<'
getMatchingBracket _ = ' '

isOpenedBracket :: Char -> Bool
isOpenedBracket c = c == '(' || c == '[' || c == '<' || c == '{'

partOne :: [String] -> Int
partOne ls = sum $ map (\el -> if el == ')' then 3 else if el == ']' then 57 else if el == '}' then 1197 else 25137) badBracketInLines where
  badBracketInLines = map (\(Just a) -> a) $ filter isJust $ map (getBadBracket [] Nothing) ls

getBracketValue :: Char -> Int
getBracketValue ')' = 1
getBracketValue ']' = 2
getBracketValue '}' = 3
getBracketValue '>' = 4
getBracketValue _ = 0


getLineValue :: String -> Int
getLineValue l = foldl (\acc c -> acc * 5 + getBracketValue c) 0 (reverse l)

getMiddleIndex :: Int -> Int
getMiddleIndex n = floor (fromIntegral (n - 1) / 2)

partTwo :: [String] -> Int
partTwo ls = autoCompletesSum !! getMiddleIndex (length autoCompletesSum) where
  filteredLines = filter (isNothing . getBadBracket [] Nothing) ls
  autocompleteLists = map completeLine filteredLines
  autoCompletesSum = sort $ map getLineValue autocompleteLists
  completeLine l = getCompletion l
  getCompletion l = map getMatchingBracket (reverse stack) where
    stack = foldl (\acc c -> if isOpenedBracket c then c:acc else if null acc then acc else tail acc) [] l

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let ls = lines inp

  let sumOfBadBrackets = partOne ls
  let middleSum = partTwo ls

  writeFile "output.txt" $ show sumOfBadBrackets ++ "\n" ++ show middleSum

  return ()