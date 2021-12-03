module Day3 where

import Numeric
import Data.List
import Data.Ord

part1 :: [String] -> Int
part1 input = (gamma input) * (epsilon input)

part2 :: [String] -> Int
part2 input = (oxygen input) * (co2 input)

gamma :: [String] -> Int
gamma = binToInt . mostCommon
 
epsilon :: [String] -> Int
epsilon = binToInt . leastCommon

oxygen :: [String] -> Int
oxygen = binToInt . go 0
    where
        go _ [x] = x
        go n xs = go (n+1) (filter (\x -> x!!n == (mostCommon xs)!!n) xs) 

co2 :: [String] -> Int
co2 = binToInt . go 0
    where
        go _ [x] = x
        go n xs = go (n+1) (filter (\x -> x!!n == (leastCommon xs)!!n) xs)

mostCommon :: [String] -> String
mostCommon = map (head . maximumBy (comparing length) . group . sort) . transpose

leastCommon :: [String] -> String
leastCommon = map (head . minimumBy (comparing length) . group . sort) . transpose

binToInt :: String -> Int 
binToInt = fst . head . readBin 

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input