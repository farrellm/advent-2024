{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Advent
import Control.Monad.ST (ST, runST)
import Data.HashSet qualified as HS
import Data.HashTable.ST.Basic qualified as H
import Data.Map.Strict qualified as M
import Data.Vector.Hashtables qualified as HV
import Data.Vector.Unboxed.Mutable qualified as VM
import Relude.Unsafe qualified as U

data Day6 = Day6
  { pos :: CxInt,
    dir :: CxInt,
    past :: HashSet (CxInt)
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
      f = executingState Day6 {pos = p, dir = 0 :+: 1, past = HS.singleton p} go
      go = do
        x <- (+) <$> use #pos <*> use #dir
        if
          | realPart x < j1
              || realPart x > j2
              || imagPart x < i1
              || imagPart x > i2 ->
              pass
          | HS.member x walls -> do
              #dir %= (* (0 :+: (-1)))
              go
          | otherwise -> do
              #pos .= x
              #past %= HS.insert x
              go

  print $ HS.size f.past

data Part2 = Part2
  { pos :: !(CxInt),
    dir :: !(CxInt),
    past :: !(HashSet (CxInt, CxInt))
  }
  deriving (Generic, Show)

part2 :: IO ()
part2 = do
  (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input6.txt"
  let p = U.fromJust . viaNonEmpty head . toList $ g M.! '^'
      (i1, i2, j1, j2) = gridRange g
      walls = U.fromJust $ M.lookup '#' g
      d0 = 0 :+: 1
      f =
        filter
          ( evaluatingState
              Part2 {pos = p, dir = d0, past = HS.singleton (p, d0)}
              . go
          )
          . toList
          $ g M.! '.'

      go :: CxInt -> State Part2 Bool
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
              modifying' #dir (* (0 :+: (-1)))
              go block
          | otherwise -> do
              #pos .= x
              modifying' #past (HS.insert (x, d))
              go block

  print $ length f

data Part2H s = Part2H
  { pos :: !(CxInt),
    dir :: !(CxInt),
    past :: !(H.HashTable s (CxInt, CxInt) ())
  }
  deriving (Generic, Show)

part2h :: IO ()
part2h = do
  (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input6.txt"
  let p = U.fromJust . viaNonEmpty head . toList $ g M.! '^'
      (i1, i2, j1, j2) = gridRange g
      walls = U.fromJust $ M.lookup '#' g
      d0 = 0 :+: 1
      f =
        filter
          ( \x -> runST $ do
              h <- H.newSized 10000
              evaluatingStateT
                Part2H {pos = p, dir = d0, past = h}
                $ go x
          )
          . toList
          $ g M.! '.'

      go :: forall s. CxInt -> StateT (Part2H s) (ST s) Bool
      go block = do
        x <- (+) <$> use #pos <*> use #dir
        d <- use #dir
        s <- use #past
        seen <- isJust <$> lift (H.lookup s (x, d))
        if
          | seen -> pure True
          | realPart x < j1
              || realPart x > j2
              || imagPart x < i1
              || imagPart x > i2 ->
              pure False
          | x == block || HS.member x walls -> do
              modifying' #dir (* (0 :+: (-1)))
              go block
          | otherwise -> do
              #pos .= x
              lift $ H.insert s (x, d) ()
              go block

  print $ length f

type HashTable s k v = HV.Dictionary s VM.MVector k VM.MVector v

data Part2HV s = Part2HV
  { pos :: !(CxInt),
    dir :: !(CxInt)
  }
  deriving (Generic)

part2hv :: IO ()
part2hv = do
  -- (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input6.txt"
  (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input6.txt"
  let p = U.fromJust . viaNonEmpty head . toList $ g M.! '^'
      (i1, i2, j1, j2) = gridRange g
      walls = U.fromJust $ M.lookup '#' g
      d0 = 0 :+: 1

      q = runST $ do
        h <- HV.initialize 0
        void
          $ evaluatingStateT
            Part2HV {pos = p, dir = d0}
          $ go ((-1 :+: 1), h)
        HV.toList h

      f =
        filter
          ( \x -> runST $ do
              h <- HV.initialize 0
              evaluatingStateT
                Part2HV {pos = p, dir = d0}
                $ go (x, h)
          )
          -- . toList
          -- \$ g M.! '.'
          . HS.toList
          $ HS.fromList (fst . fst <$> q)

      go ::
        forall s.
        (CxInt, HashTable s (CxInt, CxInt) ()) ->
        StateT (Part2HV s) (ST s) Bool
      go (block, s) = do
        x <- (+) <$> use #pos <*> use #dir
        d <- use #dir
        seen <- isJust <$> lift (HV.lookup s (x, d))
        if
          | seen -> pure True
          | realPart x < j1
              || realPart x > j2
              || imagPart x < i1
              || imagPart x > i2 ->
              pure False
          | x == block || HS.member x walls -> do
              modifying' #dir (* (0 :+: (-1)))
              go (block, s)
          | otherwise -> do
              #pos .= x
              lift $ HV.insert s (x, d) ()
              go (block, s)

  print $ length f
