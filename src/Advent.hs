module Advent
  ( module X,
    Parser,
    unsafeParse,
    decimal,
    someNE,
  )
where

import Control.Monad.Combinators.NonEmpty as X hiding (some, someTill)
import Control.Monad.Combinators.NonEmpty qualified as NE
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
