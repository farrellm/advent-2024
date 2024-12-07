{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Advent
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Set qualified as S

pair :: Parser (Int, Int)
pair = (,) <$> decimal <*> ("|" *> decimal)

part1 :: IO ()
part1 = do
  (a, b) <-
    break (== "") . lines . decodeUtf8
      <$> readFileBS "data/input5.txt"
  let xs = mapMaybe (parseMaybe pair) a
      ys = mapMaybe (parseMaybe (decimal `sepBy` ",")) $ drop 1 b

      isGood [] = True
      isGood (p : ps) =
        (not $ any (flip elem xs . (,p)) ps) && isGood ps

      mid l = l !!? (length l `div` 2)
  print . sum . mapMaybe mid $ filter isGood ys

part2 :: IO ()
part2 = do
  (a, b) <-
    break (== "") . lines . decodeUtf8
      <$> readFileBS "data/input5.txt"
  let xs = S.fromList $ mapMaybe (parseMaybe pair) a
      ys = mapMaybe (parseMaybe (decimal `sepBy` ",")) $ drop 1 b

      z =
        foldl'
          ( \m (k, v) ->
              M.alter (Just . maybe (IS.singleton v) (IS.insert v)) k m
          )
          mempty
          xs

      isGood [] = True
      isGood (p : ps) =
        (not $ any (flip S.member xs . (,p)) ps) && isGood ps

      tsort ls
        | IS.null ls = []
        | otherwise =
            let s =
                  foldl'
                    (\r v -> IS.difference r . fromMaybe IS.empty $ z M.!? v)
                    ls
                    (IS.toList ls)
                e = IS.findMin s
             in e : tsort (IS.delete e ls)

      mid l = l !!? (length l `div` 2)
  print
    . sum
    . mapMaybe mid
    . fmap (tsort . IS.fromList)
    $ filter (not . isGood) ys
