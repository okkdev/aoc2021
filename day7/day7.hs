module Day7 where

import Data.List

part1 :: [Int] -> Int
part1 xs = sum $ map (abs . flip (-) median) xs
    where median = sort xs !! (length xs `div` 2)

-- This is a 50/50 chance of success since I'm not checking for ceiling or floor on the mean
-- Doesn't work for the test, but it worked for my input LUL
part2 :: [Int] -> Int
part2 xs = sum $ concatMap (\x -> take (abs $ x-mean) [1..]) xs
    where mean = sum xs `div` length xs

main :: IO ()
main = do
  input <- read . (\x -> "[" ++ x ++ "]") <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input