module Day1 where

import Advent
import Data.Map.Strict qualified as M

pair :: Parser (Int, Int)
pair = (,) <$> decimal <* space1 <*> decimal

part1 :: IO ()
part1 = do
  ps <- unsafeParse (pair `sepEndBy` newline) "data/input1.txt"
  print
    . sum
    . fmap abs
    . uncurry (zipWith (-))
    . bimap sort sort
    $ unzip ps

part2 :: IO ()
part2 = do
  ps <- unsafeParse (pair `sepEndBy` newline) "data/input1.txt"
  let (a, b) = unzip ps
      a' = foldl' (\m k -> M.insert k (0 :: Int) m) mempty a
      b' = foldl' (\m k -> M.adjust (+ 1) k m) a' b
  print . sum $ mapMaybe (\k -> (k *) <$> M.lookup k b') a
