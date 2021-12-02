module Day2 where

import Data.List

part1 :: [String] -> Int
part1 = uncurry (*) . parse (0, 0)

parse :: (Int, Int) -> [String] -> (Int, Int)
parse acc [] = acc
parse (p,d) (x:xs) | "forward" `isPrefixOf` x = parse (p + (parseNum x), d) xs
                   | "down" `isPrefixOf` x = parse (p, d + (parseNum x)) xs
                   | "up" `isPrefixOf` x = parse (p, d - (parseNum x)) xs
                        where parseNum = read . pure . last

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
--   putStrLn "Part 2:"
--   print $ part2 input