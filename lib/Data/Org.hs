{-# LANGUAGE DerivingStrategies #-}

module Data.Org
  ( -- * Types
    Org(..)
  , Words(..)
  , URL(..)
  , Language(..)
    -- * Parser
  , org
    -- * Pretty Printing
  , prettyOrgs
  , prettyOrg
  , prettyWords
  ) where

import           Control.Applicative.Combinators.NonEmpty
import           Control.Monad (void)
import qualified Data.List.NonEmpty as NEL
import           Data.Semigroup (sconcat)
import           Data.Text (Text)
import qualified Data.Text as T
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
org = many (choice [try heading, try code, try example, try quote, paragraph]) <* eof

heading :: Parser Org
heading = L.lexeme space $ do
  stars <- some $ single '*'
  void $ single ' '
  Heading (NEL.length stars) <$> line

quote :: Parser Org
quote = L.lexeme space $ do
  void top <* newline
  ls <- manyTill (takeWhileP Nothing (/= '\n') <* newline) bot
  pure . Quote $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_QUOTE" <|> string "begin_quote")
    bot = string "#+" *> (string "END_QUOTE" <|> string "end_quote")

example :: Parser Org
example = L.lexeme space $ do
  void top <* newline
  ls <- manyTill (takeWhileP Nothing (/= '\n') <* newline) bot
  pure . Example $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_EXAMPLE" <|> string "begin_example")
    bot = string "#+" *> (string "END_EXAMPLE" <|> string "end_example")

code :: Parser Org
code = L.lexeme space $ do
  lang <- top *> optional lng <* newline
  ls <- manyTill (takeWhileP Nothing (/= '\n') <* newline) bot
  pure . Code (Language <$> lang) $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_SRC" <|> string "begin_src")
    bot = string "#+" *> (string "END_SRC" <|> string "end_src")
    lng = single ' '  *> takeWhile1P Nothing (/= '\n')

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

link :: Parser Words
link = between (single '[') (single ']') $ Link
  <$> between (single '[') (single ']') (URL <$> takeWhile1P Nothing (/= ']'))
  <*> optional (between (single '[') (single ']') (takeWhile1P Nothing (/= ']')))

-- meta :: Parser Org
-- meta = undefined

--------------------------------------------------------------------------------
-- Pretty Printing

prettyOrgs :: [Org] -> Text
prettyOrgs = T.intercalate "\n\n" . map prettyOrg

prettyOrg :: Org -> Text
prettyOrg o = case o of
  Heading n ws -> T.unwords $ T.replicate n "*" : NEL.toList (NEL.map prettyWords ws)
  Paragraph ws -> T.unwords . NEL.toList $ NEL.map prettyWords ws
  _            -> ""

prettyWords :: Words -> Text
prettyWords w = case w of
  Bold t                  -> "*" <> t <> "*"
  Italic t                -> "/" <> t <> "/"
  Highlight t             -> "~" <> t <> "~"
  Underline t             -> "_" <> t <> "_"
  Verbatim t              -> "=" <> t <> "="
  Strike t                -> "+" <> t <> "+"
  Link (URL url) Nothing  -> "[[" <> url <> "]]"
  Link (URL url) (Just t) -> "[[" <> url <> "][" <> t <> "]]"
  Image (URL url)         -> "[[" <> url <> "]]"
  Plain t                 -> t
