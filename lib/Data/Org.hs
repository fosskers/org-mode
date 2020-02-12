{-# LANGUAGE DerivingStrategies #-}

module Data.Org
  ( -- * Types
    Org(..)
  , Words(..)
  , URL(..)
  , Language(..)
    -- * Parser
  , org
  ) where

import           Control.Applicative.Combinators.NonEmpty
import           Control.Monad (void)
import qualified Data.List.NonEmpty as NEL
import           Data.Semigroup (sconcat)
import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec hiding (sepBy1, sepEndBy1, some, someTill)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- Types

-- | An org-mode document tree.
data Org
  = Heading Int (NEL.NonEmpty Words)
  | Quote Text
  | Example Text
  | Code (Maybe Language) Text
  | Paragraph (NEL.NonEmpty Words)
  deriving stock (Eq, Show)

data Words
  = Bold Text
  | Italic Text
  | Highlight Text
  | Underline Text
  | Verbatim Text
  | Strike Text
  | Link URL (Maybe Text)
  | Image URL
  | Plain Text
  deriving stock (Eq, Show)

-- | The url portion of a link.
newtype URL = URL Text deriving stock (Eq, Show)

-- | The programming language some source code block was written in.
newtype Language = Language Text deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void Text

-- TODO Handle whitespace.
org :: Parser [Org]
-- org = many (choice [heading, quote, example, code, paragraph]) <* eof
org = many (choice [try heading, paragraph]) <* eof

heading :: Parser Org
heading = L.lexeme space $ do
  stars <- some $ single '*'
  void $ single ' '
  l <- line
  pure $ Heading (NEL.length stars) l

quote :: Parser Org
quote = undefined

example :: Parser Org
example = undefined

code :: Parser Org
code = undefined

paragraph :: Parser Org
paragraph = L.lexeme space $ Paragraph . sconcat <$> sepEndBy1 line newline

line :: Parser (NEL.NonEmpty Words)
line = some wordChunk

wordChunk :: Parser Words
wordChunk = choice
  [ Bold      <$> between (single '*') (single '*') (takeWhile1P Nothing (/= '*'))
  , Italic    <$> between (single '/') (single '/') (takeWhile1P Nothing (/= '/'))
  , Highlight <$> between (single '~') (single '~') (takeWhile1P Nothing (/= '~'))
  , Verbatim  <$> between (single '=') (single '=') (takeWhile1P Nothing (/= '='))
  , Underline <$> between (single '_') (single '_') (takeWhile1P Nothing (/= '_'))
  , Strike    <$> between (single '+') (single '+') (takeWhile1P Nothing (/= '+'))
  , try image
  , link
  , Plain     <$> takeWhile1P Nothing (/= '\n')
  ]

image :: Parser Words
image = between (single '[') (single ']') $
  between (single '[') (single ']') $ do
    path <- takeWhile1P Nothing (/= '.')
    void $ single '.'
    ext <- string "jpg" <|> string "jpeg" <|> string "png"
    pure . Image . URL $ path <> "." <> ext

-- | TODO Image support. For lucid conversion, one will be `a` and the other
-- will be an `img` element.
link :: Parser Words
link = between (single '[') (single ']') $ Link
  <$> between (single '[') (single ']') (URL <$> takeWhile1P Nothing (/= ']'))
  <*> optional (between (single '[') (single ']') (takeWhile1P Nothing (/= ']')))

-- meta :: Parser Org
-- meta = undefined
