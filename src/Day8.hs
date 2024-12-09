module Day8 where

import Advent
import Data.HashSet qualified as S
import Data.Map.Strict qualified as M

stuff :: Parser ()
stuff = pure ()

part1 :: IO ()
part1 = do
  (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input8.txt"
  let r = gridRange g
      g' = M.delete '.' g
      a = do
        zs <- M.elems g'
        z1 <- S.toList zs
        z2 <- S.toList zs
        guard (z1 /= z2)
        let p = z1 + z1 - z2
        guard (inGrid r p)
        pure p
  printGrid (M.insert '#' (S.fromList a) g')
  print (S.size $ S.fromList a)

part2 :: IO ()
part2 = do
  (_, g) <- charGrid . decodeUtf8 <$> readFileBS "data/input8.txt"
  let r = gridRange g
      g' = M.delete '.' g
      a = do
        zs <- M.elems g'
        z1 <- S.toList zs
        z2 <- S.toList zs
        guard (z1 /= z2)
        let d = z2 - z1
        takeWhile (inGrid r) $ iterate (+ d) z1
  printGrid (M.insert '#' (S.fromList a) g')
  print (S.size $ S.fromList a)
