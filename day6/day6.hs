module Day6 where

import Data.List

part1 :: [Int] -> Int
part1 = getFishesAtFutureDay 80

part2 :: [Int] -> Int
part2 = getFishesAtFutureDay 256

getFishesAtFutureDay :: Int -> [Int] -> Int
getFishesAtFutureDay n = sum . (!! n) . iterate nextDay . fishSchools

nextDay :: [Int] -> [Int]
nextDay [d0, d1, d2, d3, d4, d5, d6, d7, d8] = 
  [d1, d2, d3, d4, d5, d6, d7 + d0, d8, d0]

fishSchools :: [Int] -> [Int]
fishSchools = map (pred . length) . group . sort . (++ [0..8])

main :: IO ()
main = do
  input <- read . (\x -> "[" ++ x ++ "]") <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input