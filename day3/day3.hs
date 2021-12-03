module Day2 where

import Numeric
import Data.List
import Data.Ord

part1 :: [String] -> Int
part1 input =
    (gamma input') * (epsilon input')
    where 
        input' = transpose input
        gamma = toInt . map (head . maximumBy (comparing length) . group . sort)
        epsilon = toInt . map (head . minimumBy (comparing length) . group . sort)

toInt :: String -> Int 
toInt = fst . head . readBin 

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
--   putStrLn "Part 2:"
--   print $ part2 input