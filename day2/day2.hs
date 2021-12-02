module Day2 where

import Data.List

part1 :: [String] -> Int
part1 = uncurry (*) . parse1 (0, 0)

parse1 :: (Int, Int) -> [String] -> (Int, Int)
parse1 acc [] = acc
parse1 (p,d) (x:xs) | "forward" `isPrefixOf` x = parse1 (p + (parseNum x), d) xs
                    | "down" `isPrefixOf` x = parse1 (p, d + (parseNum x)) xs
                    | "up" `isPrefixOf` x = parse1 (p, d - (parseNum x)) xs

part2 :: [String] -> Int
part2 = uncurry (*) . parse2 (0, 0, 0)

parse2 :: (Int, Int, Int) -> [String] -> (Int, Int)
parse2 (p,d,a) [] = (p, d)
parse2 (p,d,a) (x:xs) | "forward" `isPrefixOf` x = parse2 (p + x', d + (a * x'), a) xs
                      | "down" `isPrefixOf` x = parse2 (p, d, a + x') xs
                      | "up" `isPrefixOf` x = parse2 (p, d, a - x') xs
                        where x' = parseNum x

parseNum :: String -> Int
parseNum = read . pure . last

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input