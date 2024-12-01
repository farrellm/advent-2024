module Advent
  ( module X,
    Parser,
    unsafeParse,
    decimal,
  )
where

import Text.Megaparsec as X
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
