{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Advent
  ( module X,
    Parser,
    unsafeParse,
    decimal,
    someNE,
    Dir4 (..),
    Dir8 (..),
    ToComplex (..),
    dir4,
    dir8,
    charGrid,
    findGridChar,
    gridRange,
    printGrid,
    CxInt (..),
    realPart,
    imagPart,
  )
where

import Control.Monad.Combinators.NonEmpty as X hiding (some, someTill)
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Foldable (Foldable (minimum), maximum)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Map.Strict qualified as M
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Optics as X hiding (noneOf)
import Optics.State.Operators as X
import Text.Megaparsec as X hiding
  ( State (..),
    endBy1,
    many,
    sepBy1,
    sepEndBy1,
    some,
  )
import Text.Megaparsec.Char as X
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

unsafeParse :: Parser a -> FilePath -> IO a
unsafeParse p f = do
  res <- parse p f . decodeUtf8 <$> readFileBS f
  case res of
    Right a -> pure a
    Left e -> error . toText $ errorBundlePretty e

decimal :: Parser Int
decimal = L.signed space1 L.decimal

someNE :: (MonadPlus m) => m a -> m (NonEmpty a)
someNE = NE.some
{-# INLINE someNE #-}

data Dir4 = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)

data Dir8 = NN | NE | EE | SE | SS | SW | WW | NW
  deriving (Show, Eq, Ord, Enum, Bounded)

class ToComplex a where
  toCx :: a -> CxInt

instance ToComplex Dir4 where
  toCx N = 0 :+: 1
  toCx E = 1 :+: 0
  toCx S = 0 :+: (-1)
  toCx W = (-1) :+: 0

instance ToComplex Dir8 where
  toCx NN = 0 :+: 1
  toCx NE = 1 :+: 1
  toCx EE = 1 :+: 0
  toCx SE = 1 :+: (-1)
  toCx SS = 0 :+: (-1)
  toCx SW = (-1) :+: (-1)
  toCx WW = (-1) :+: 0
  toCx NW = (-1) :+: 1

dir4 :: [CxInt]
dir4 = toCx <$> universe @Dir4

dir8 :: [CxInt]
dir8 = toCx <$> universe @Dir8

charGrid :: Text -> (HashMap (CxInt) Char, Map Char (HashSet (CxInt)))
charGrid t =
  let xs = do
        (i, l) <- zip [0, -1 ..] $ lines t
        (j, c) <- zip [0 ..] $ toString l
        pure (j :+: i, c)
   in ( HM.fromList xs,
        foldl'
          ( \m (z, c) ->
              M.alter
                (Just . maybe (HS.singleton z) (HS.insert z))
                c
                m
          )
          M.empty
          xs
      )

findGridChar :: CxInt -> Map Char (HashSet (CxInt)) -> Maybe Char
findGridChar z =
  fmap fst
    . find (HS.member z . snd)
    . M.toList

gridRange :: Map Char (HashSet (CxInt)) -> (Int, Int, Int, Int)
gridRange g =
  let zs = concat . fmap HS.toList $ M.elems g
      i1 = minimum $ imagPart <$> zs
      i2 = maximum $ imagPart <$> zs
      j1 = minimum $ realPart <$> zs
      j2 = maximum $ realPart <$> zs
   in (i1, i2, j1, j2)

printGrid :: Map Char (HashSet (CxInt)) -> IO ()
printGrid g = do
  let (i1, i2, j1, j2) = gridRange g
  for_ (reverse [i1 .. i2]) $ \i -> do
    for_ [j1 .. j2] $ \j -> do
      case findGridChar (j :+: i) g of
        Nothing -> putStr "_"
        Just c -> putStr [c]
    putStrLn ""

data CxInt = !Int :+: !Int
  deriving (Generic, Show, Eq)

instance Num CxInt where
  (x :+: y) + (x' :+: y') = (x + x') :+: (y + y')
  {-# INLINE (+) #-}
  (x :+: y) - (x' :+: y') = (x - x') :+: (y - y')
  {-# INLINE (-) #-}
  (x :+: y) * (x' :+: y') = (x * x' - y * y') :+: (x * y' + y * x')
  {-# INLINE (*) #-}
  negate (x :+: y) = negate x :+: negate y
  {-# INLINE negate #-}
  abs _ = undefined
  signum (0 :+: 0) = 0
  signum _ = undefined
  fromInteger n = fromInteger n :+: 0

instance Hashable CxInt

realPart :: CxInt -> Int
realPart (x :+: _) = x
{-# INLINE realPart #-}

imagPart :: CxInt -> Int
imagPart (_ :+: y) = y
{-# INLINE imagPart #-}

derivingUnbox
  "CxInt"
  [t|CxInt -> (Int, Int)|]
  [|\(r :+: i) -> (r, i)|]
  [|\(r, i) -> r :+: i|]
