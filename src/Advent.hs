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
  )
where

import Control.Monad.Combinators.NonEmpty as X hiding (some, someTill)
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Complex as X
import Optics as X hiding (noneOf)
import Text.Megaparsec as X hiding (endBy1, many, sepBy1, sepEndBy1, some)
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
  toCx :: a -> Complex Int

instance ToComplex Dir4 where
  toCx N = 0 :+ (-1)
  toCx E = 1 :+ 0
  toCx S = 0 :+ 1
  toCx W = (-1) :+ 0

instance ToComplex Dir8 where
  toCx NN = 0 :+ (-1)
  toCx NE = 1 :+ (-1)
  toCx EE = 1 :+ 0
  toCx SE = 1 :+ 1
  toCx SS = 0 :+ 1
  toCx SW = (-1) :+ 1
  toCx WW = (-1) :+ 0
  toCx NW = (-1) :+ (-1)

dir4 :: [Complex Int]
dir4 = toCx <$> universe @Dir4

dir8 :: [Complex Int]
dir8 = toCx <$> universe @Dir8
