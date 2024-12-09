{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Advent

stuff :: Parser (Int, NonEmpty Int)
stuff = (,) <$> decimal <*> (":" *> hspace1 *> (decimal `sepBy1` hspace1))

part1 :: IO ()
part1 = do
  ps <- unsafeParse (stuff `sepEndBy` newline) "data/input7.txt"
  print . sum . fmap fst $ filter isGood ps
  where
    isGood (n, x :| []) = n == x
    isGood (n, x :| (y : ys))
      | x > n = False
      | otherwise = isGood (n, x + y :| ys) || isGood (n, x * y :| ys)

part2 :: IO ()
part2 = do
  ps <- unsafeParse (stuff `sepEndBy` newline) "data/input7.txt"
  print . sum . fmap fst $ filter isGood ps
  where
    isGood (n, x :| []) = n == x
    isGood (n, x :| (y : ys))
      | x > n = False
      | otherwise =
          isGood (n, x + y :| ys)
            || isGood (n, x * y :| ys)
            || isGood (n, x `ci` y :| ys)

    ci x y = x * go 10 y + y

    go i n
      | i > n = i
      | otherwise = go (i * 10) n
