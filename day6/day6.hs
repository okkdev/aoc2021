module Day6 where

part1 :: [Int] -> Int
part1 = length . days 80

part2 :: [Int] -> Int
part2 = length . days 256

days :: Int -> [Int] -> [Int]
days 0 xs = xs
days n xs = days (n-1) (concat $ map (\x -> if x == 0 then [6, 8] else [x-1]) xs)

main :: IO ()
main = do
  input <- read . (\x -> "[" ++ x ++ "]") <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input