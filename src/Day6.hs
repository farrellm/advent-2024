{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Advent
import Data.HashSet qualified as HS
import Data.Map.Strict qualified as M
import Relude.Unsafe qualified as U

data Day6 = Day6
  { pos :: Complex Int,
    dir :: Complex Int,
    past :: HashSet (Complex Int)
  }
  deriving (Generic, Show)

stuff :: Parser ()
stuff = pure ()

part1 :: IO ()
part1 = do
  (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input6.txt"
  let p = U.fromJust . viaNonEmpty head . toList $ g M.! '^'
      (i1, i2, j1, j2) = gridRange g
      walls = U.fromJust $ M.lookup '#' g
      f = executingState Day6 {pos = p, dir = 0 :+ 1, past = HS.singleton p} go
      go = do
        x <- (+) <$> use #pos <*> use #dir
        if
          | realPart x < j1
              || realPart x > j2
              || imagPart x < i1
              || imagPart x > i2 ->
              pass
          | HS.member x walls -> do
              #dir %= (* (0 :+ (-1)))
              go
          | otherwise -> do
              #pos .= x
              #past %= HS.insert x
              go

  print $ HS.size f.past

data Part2 = Part2
  { pos :: !(Complex Int),
    dir :: !(Complex Int),
    past :: !(HashSet (Complex Int, Complex Int))
  }
  deriving (Generic, Show)

part2 :: IO ()
part2 = do
  (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input6.txt"
  let p = U.fromJust . viaNonEmpty head . toList $ g M.! '^'
      (i1, i2, j1, j2) = gridRange g
      walls = U.fromJust $ M.lookup '#' g
      d0 = 0 :+ 1
      f =
        filter
          ( evaluatingState
              Part2 {pos = p, dir = d0, past = HS.singleton (p, d0)}
              . go
          )
          . toList
          $ g M.! '.'

      go :: Complex Int -> State Part2 Bool
      go block = do
        x <- (+) <$> use #pos <*> use #dir
        d <- use #dir
        s <- use #past
        if
          | HS.member (x, d) s -> pure True
          | realPart x < j1
              || realPart x > j2
              || imagPart x < i1
              || imagPart x > i2 ->
              pure False
          | x == block || HS.member x walls -> do
              #dir %= (* (0 :+ (-1)))
              go block
          | otherwise -> do
              #pos .= x
              #past %= HS.insert (x, d)
              go block

  print $ length f
