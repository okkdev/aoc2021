module Day01 where

part1 :: [Int] -> Int
part1 xs = foldl (\acc (x, y) -> if x < y then acc+1 else acc) 0 . zip xs $ tail xs

part2 :: [Int] -> Int
part2 = part1 . go
  where 
    go [] = []
    go xs = sum (take 3 xs) : go (drop 1 xs)

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
