module Day2 where

import Advent

stuff :: Parser (NonEmpty Int)
stuff = decimal `sepBy1` hspace1

part1 :: IO ()
part1 = do
  ps <- unsafeParse (stuff `sepEndBy` newline) "data/input2.txt"
  print . length $ filter isSafe ps

isSafe :: NonEmpty Int -> Bool
isSafe =
  (all ((1 <=) &&^ (<= 3)) ||^ all ((-3 <=) &&^ (<= -1)))
    . uncurry (zipWith (-))
    . ((toList @NonEmpty) &&& tail)

part2 :: IO ()
part2 = do
  ps <- unsafeParse (stuff `sepEndBy` newline) "data/input2.txt"
  print . length $ filter isSafe' ps
  where
    isSafe' :: NonEmpty Int -> Bool
    isSafe' = any isSafe . drop1

drop1 :: NonEmpty Int -> [NonEmpty Int]
drop1 f@(x :| xs) = f : mapMaybe nonEmpty (go (x : xs))
  where
    go :: [a] -> [[a]]
    go [] = []
    go (y : ys) = ys : fmap (y :) (go ys)
