{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DayX where

import Advent
import Data.HashMap.Strict qualified as H
import Data.Map.Strict qualified as M

stuff :: Parser ()
stuff = pure ()

part1 :: IO ()
part1 = do
  ps <- unsafeParse (stuff `sepEndBy` newline) "data/testX.txt"
  print ps

part2 :: IO ()
part2 = do
  ps <- unsafeParse (stuff `sepEndBy` newline) "data/testX.txt"
  print ps
