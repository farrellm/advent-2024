{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Advent
import Prelude hiding (Op)

parens :: Parser a -> Parser a
parens x = "(" *> x <* ")"

pair :: Parser (Int, Int)
pair = "mul" *> parens ((,) <$> decimal <*> (char ',' *> decimal))

pair' :: Parser (Maybe (Int, Int))
pair' = (Just <$> try pair) <|> (anySingle *> pure Nothing)

part1 :: IO ()
part1 = do
  ps <- unsafeParse (some pair') "data/input3.txt"
  print . sum $ (uncurry (*)) <$> catMaybes ps

data Op = Do | Dont | Mul Int Int
  deriving (Show)

op :: Parser Op
op =
  try ("do()" *> pure Do)
    <|> try ("don't()" *> pure Dont)
    <|> ("mul" *> parens (Mul <$> decimal <*> (char ',' *> decimal)))

op' :: Parser (Maybe Op)
op' = (Just <$> try op) <|> (anySingle *> pure Nothing)

part2 :: IO ()
part2 = do
  ps <- unsafeParse (some op') "data/input3.txt"
  print . goDo $ catMaybes ps
  where
    goDo [] = 0
    goDo (Do : xs) = goDo xs
    goDo (Dont : xs) = goDont xs
    goDo (Mul a b : xs) = (a * b) + goDo xs

    goDont [] = 0
    goDont (Do : xs) = goDo xs
    goDont (Dont : xs) = goDont xs
    goDont (Mul _ _ : xs) = goDont xs
