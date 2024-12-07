{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.Text qualified as T
import Data.Vector ((!))
import Data.Vector qualified as V

searchLine :: Text -> Int
searchLine =
  length
    . filter (T.isPrefixOf "XMAS" ||^ T.isPrefixOf "SAMX")
    . T.tails

mkDiag1 :: [Text] -> [Text]
mkDiag1 rs =
  let n = length rs - 1
      go i r =
        T.concat [toText (replicate i '.'), r, toText (replicate (n - i) '.')]
   in zipWith go [0 ..] rs

mkDiag2 :: [Text] -> [Text]
mkDiag2 rs =
  let n = length rs - 1
      go i r =
        T.concat [toText (replicate (n - i) '.'), r, toText (replicate i '.')]
   in zipWith go [0 ..] rs

part1 :: IO ()
part1 = do
  rs <- lines . decodeUtf8 <$> readFileBS "data/input4.txt"
  let cs = T.transpose rs
      d1 = T.transpose $ mkDiag1 rs
      d2 = T.transpose $ mkDiag2 rs
  print . sum $
    [ sum $ searchLine <$> rs,
      sum $ searchLine <$> cs,
      sum $ searchLine <$> d1,
      sum $ searchLine <$> d2
    ]

part2 :: IO ()
part2 = do
  g <-
    V.fromList . fmap (V.fromList . toString) . lines . decodeUtf8
      <$> readFileBS "data/input4.txt"
  let m = length g
      n = length $ V.head g
  print . sum $ do
    i <- [1 .. m - 2]
    j <- [1 .. n - 2]
    guard (g ! i ! j == 'A')
    guard
      ( g ! (i + 1) ! (j + 1) == 'M' && g ! (i - 1) ! (j - 1) == 'S'
          || g ! (i + 1) ! (j + 1) == 'S' && g ! (i - 1) ! (j - 1) == 'M'
      )
    guard
      ( g ! (i - 1) ! (j + 1) == 'M' && g ! (i + 1) ! (j - 1) == 'S'
          || g ! (i - 1) ! (j + 1) == 'S' && g ! (i + 1) ! (j - 1) == 'M'
      )
    pure (1 :: Int)
