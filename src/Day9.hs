{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Advent

newtype ID = ID {getID :: Int}
  deriving (Show, Eq, Ord)

stuff :: Parser [Either Int (Int, ID)]
stuff =
  addID 0 . concat
    <$> many
      ( (:)
          <$> (Right . digInt <$> digitChar)
          <*> (pure . Left . digInt <$> digitChar <|> pure [])
      )
  where
    digInt c = ord c - ord '0'
    addID _ [] = []
    addID i (Right n : rs) = Right (n, ID i) : addID (i + 1) rs
    addID i (Left n : rs) = Left n : addID i rs

data P1 = P1
  { front :: ![Either Int (Int, ID)],
    back :: ![Either Int (Int, ID)],
    i :: !Int,
    fn :: !Int,
    fid :: !ID,
    fi :: !Int,
    bi :: !Int,
    bn :: !Int,
    bid :: !ID,
    res :: !Int
  }
  deriving (Generic, Show)

goFront :: State P1 ()
goFront = do
  s <- get
  if s.fi < s.fn
    then do
      #res %= (+ (s.i * getID s.fid))
      #i %= (+ 1)
      #fi %= (+ 1)
      goFront
    else do
      case s.front of
        [] -> pass
        (Right (n, j) : rs) -> do
          if j >= s.bid && s.bid >= ID 0
            then do
              #fn %= (+ s.bn)
              goBack
            else do
              #fn .= n
              #fid .= j
              #fi .= 0
              #front .= rs
              goFront
        (Left n : rs) -> do
          #fn .= n
          #fi .= 0
          #front .= rs
          goBack

goBack :: State P1 ()
goBack = do
  s <- get
  if
    | s.fi == s.fn -> goFront
    | s.bi < s.bn -> do
        #res %= (+ (s.i * getID s.bid))
        #i %= (+ 1)
        #bi %= (+ 1)
        #fi %= (+ 1)
        goBack
    | otherwise -> case s.back of
        [] -> pass
        (Left _ : rs) -> do
          #back .= rs
          goBack
        (Right (n, j) : rs) -> do
          if j <= s.fid
            then pass
            else do
              #bn .= n
              #bid .= j
              #bi .= 0
              #back .= rs
              goBack

part1 :: IO ()
part1 = do
  ps <- unsafeParse stuff "data/input9.txt"
  print
    . res
    $ execState (goFront) (P1 ps (reverse ps) 0 0 (ID 0) 0 0 0 (ID (-1)) 0)

printDisk :: [Either Int (Int, ID)] -> IO ()
printDisk [] = putStrLn ""
printDisk (Right (n, ID i) : rs) = do
  for_ [1 .. n] $ \_ -> putStr $ show i
  printDisk rs
printDisk (Left n : rs) = do
  putStr "<"
  for_ [1 .. n] $ \_ -> putStr "."
  putStr ">"
  printDisk rs

printZip :: Zip (Either Int (Int, ID)) -> IO ()
printZip (as, bs) = do
  printDisk $ reverse as
  printDisk bs
  putStrLn ""

type Zip a = ([a], [a])

mkZip :: [a] -> Zip a
mkZip = ([],)

unZip :: Zip a -> [a]
unZip (as, bs) = reverse as ++ bs

zipNext :: Zip a -> Zip a
zipNext z@(_, []) = z
zipNext (as, b : bs) = (b : as, bs)

zipPrev :: Zip a -> Zip a
zipPrev z@([], _) = z
zipPrev (a : as, bs) = (as, a : bs)

zipFront :: Zip a -> Zip a
zipFront z@([], _) = z
zipFront z = zipFront $ zipPrev z

zipFind :: ID -> Zip (Either Int (Int, ID)) -> Maybe Int
zipFind _ (_, []) = Nothing
zipFind i (_, Right (n, j) : _) | i == j = Just n
zipFind i z = zipFind i $ zipNext z

zipExcise ::
  ID -> Zip (Either Int (Int, ID)) -> Maybe (Zip (Either Int (Int, ID)))
zipExcise _ (_, []) = Nothing
zipExcise i (Left m1 : as, Right (n, j) : Left m2 : bs)
  | i == j = Just (as, Left (m1 + n + m2) : bs)
zipExcise i (as, Right (n, j) : Left m2 : bs)
  | i == j = Just (as, Left (n + m2) : bs)
zipExcise i (Left m1 : as, Right (n, j) : bs)
  | i == j = Just (as, Left (m1 + n) : bs)
zipExcise i (as, Right (n, j) : bs)
  | i == j = Just (as, Left n : bs)
zipExcise i z = zipExcise i $ zipNext z

zipInsert ::
  Int ->
  ID ->
  Zip (Either Int (Int, ID)) ->
  Maybe (Zip (Either Int (Int, ID)))
zipInsert _ _ (_, []) = Nothing
zipInsert _ i (_, Right (_, j) : _) | i == j = Nothing
zipInsert n i (as, Left m : bs)
  | n == m = Just (Right (n, i) : as, bs)
  | n < m = Just (Right (n, i) : as, Left (m - n) : bs)
zipInsert n i z = zipInsert n i $ zipNext z

part2 :: IO ()
part2 = do
  ps <- unsafeParse stuff "data/input9.txt"
  let is = reverse . fmap snd . catMaybes $ rightToMaybe <$> ps
      w = executingState (mkZip ps) $ do
        for_ is $ \i -> do
          z <- get
          let mz' = do
                n <- zipFind i $ zipFront z
                z1 <- zipInsert n i $ zipFront z
                zipExcise i z1
          case mz' of
            Nothing -> pass
            Just z' -> put z'
  let xs =
        fmap getID
          . concat
          . fmap (either (flip replicate (ID 0)) (uncurry replicate))
          $ unZip w
  print . sum $ zipWith (*) xs [0 ..]
