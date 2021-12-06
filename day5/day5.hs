module Day5 where

import Data.List
import qualified Data.Map.Lazy as M

type Lines = [Line]
type Line = (Point, Point)
type Point = (Int, Int) 

part1 :: Lines -> Int
part1 = countOverlaps . getPoints . filter (notDiagonal)
    where
        notDiagonal ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

part2 :: Lines -> Int
part2 = countOverlaps . getPoints

countOverlaps :: [(Point, Int)] -> Int
countOverlaps = length . M.filter (>1) . M.fromListWith (+)

getPoints :: Lines -> [(Point, Int)]
getPoints = map (\p -> (p, 1)) . concatMap (\line -> zip (xs line) (ys line))
    where
        xs ((x1, _), (x2, _)) = if x1 == x2 then repeat x1 else range x1 x2
        ys ((_, y1), (_, y2)) = if y1 == y2 then repeat y1 else range y1 y2
        range a b = [a,(a + if a > b then -1 else 1)..b]

parseInput :: String -> Lines
parseInput = map (line . words) . lines 
    where
        line x = (point $ head x, point $ last x)
        point x = read $ "(" ++ x ++ ")"

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input