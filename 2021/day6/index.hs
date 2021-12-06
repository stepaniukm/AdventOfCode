module Main where

import qualified Data.IntMap as IM

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing  = False

evolveState :: IM.IntMap Int -> Int -> IM.IntMap Int
evolveState register 0 = register
evolveState register n = evolveState updatedRegister (n-1) where
  accumulateState acc (0, value) = (IM.insertWith (+) 6 value . IM.insertWith (+) 8 value) acc
  accumulateState acc (key, value) = IM.insert (key - 1) value acc
  updatedRegister = foldl accumulateState IM.empty (reverse $ IM.toList register)

main :: IO ()
main = do
  inp <- readFile "input.txt"
  let initState = map (\el -> read el :: Int) $ words $ map (\x -> if x == ',' then ' ' else x) inp
  let initStateIntMap = foldl (\acc v -> if maybeToBool $ IM.lookup v acc then IM.insertWith (+) v 1 acc else IM.insert v 1 acc ) IM.empty initState

  let finalStateAfter80Days = evolveState initStateIntMap 80
  let amount18 = sum (IM.elems finalStateAfter80Days)

  let finalStateAfter256Days = evolveState initStateIntMap 256
  let amount256 = sum (IM.elems finalStateAfter256Days)

  writeFile "output.txt" $ show amount18 ++ "\n" ++ show amount256

  return ()