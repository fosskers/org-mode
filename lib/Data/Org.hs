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

import           Control.Applicative.Combinators.NonEmpty hiding (someTill)
import           Control.Monad (void)
import           Data.List.NonEmpty (NonEmpty(..))
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
  = Heading Int (NonEmpty Words)
  | Quote Text
  | Example Text
  | Code (Maybe Language) Text
  | List (NonEmpty (NonEmpty Words))
  | Paragraph (NonEmpty Words)
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
  | Punctuation Char
  | Plain Text
  deriving stock (Eq, Show)

-- | The url portion of a link.
newtype URL = URL Text deriving stock (Eq, Show)

-- | The programming language some source code block was written in.
newtype Language = Language Text deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void Text

org :: Parser [Org]
org = L.lexeme space
  $ many (choice [try heading, try code, try example, try quote, try list, paragraph]) <* eof

heading :: Parser Org
heading = L.lexeme space $ do
  stars <- some $ single '*'
  void $ single ' '
  Heading (NEL.length stars) <$> line

quote :: Parser Org
quote = L.lexeme space $ do
  void top <* newline
  ls <- manyTill (manyTillEnd <* newline) bot
  pure . Quote $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_QUOTE" <|> string "begin_quote")
    bot = string "#+" *> (string "END_QUOTE" <|> string "end_quote")

example :: Parser Org
example = L.lexeme space $ do
  void top <* newline
  ls <- manyTill (manyTillEnd <* newline) bot
  pure . Example $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_EXAMPLE" <|> string "begin_example")
    bot = string "#+" *> (string "END_EXAMPLE" <|> string "end_example")

code :: Parser Org
code = L.lexeme space $ do
  lang <- top *> optional lng <* newline
  ls <- manyTill (manyTillEnd <* newline) bot
  pure . Code (Language <$> lang) $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_SRC" <|> string "begin_src")
    bot = string "#+" *> (string "END_SRC" <|> string "end_src")
    lng = single ' '  *> someTillEnd

list :: Parser Org
list = L.lexeme space $ List <$> sepEndBy1 item (single '\n')

item :: Parser (NonEmpty Words)
item = string "- " *> line

paragraph :: Parser Org
paragraph = L.lexeme space $ Paragraph . sconcat <$> sepEndBy1 line newline

line :: Parser (NonEmpty Words)
line = sepBy1 wordChunk (single ' ' <|> lookAhead (oneOf punc))

-- | RULES
--
-- 1. In-lined markup is not recognized: This is not*bold*. Neither is *this*here.
-- 2. Punctuation immediately after markup close /is/ allowed: *This*, in fact, is bold.
-- 3. Otherwise, a space, newline or EOF is necessary after the close.
-- 4. Any char after a link is fine.
-- 5. When rerendering, a space must not appear between the end of a markup close and
--    a punctuation/newline character.
-- 6. But any other character must have a space before it.
wordChunk :: Parser Words
wordChunk = choice
  [ try $ Bold        <$> between (single '*') (single '*') (someTill '*') <* pOrS
  , try $ Italic      <$> between (single '/') (single '/') (someTill '/') <* pOrS
  , try $ Highlight   <$> between (single '~') (single '~') (someTill '~') <* pOrS
  , try $ Verbatim    <$> between (single '=') (single '=') (someTill '=') <* pOrS
  , try $ Underline   <$> between (single '_') (single '_') (someTill '_') <* pOrS
  , try $ Strike      <$> between (single '+') (single '+') (someTill '+') <* pOrS
  , try image
  , link
  , try $ Punctuation <$> oneOf punc
  , Plain <$> takeWhile1P Nothing (\c -> c /= ' ' && c /= '\n')
  ]
  where
    pOrS :: Parser ()
    pOrS = lookAhead $ void (oneOf $ '\n' : ' ' : punc) <|> eof

punc :: String
punc = ".,!?"

image :: Parser Words
image = between (single '[') (single ']') $
  between (single '[') (single ']') $ do
    path <- someTill '.'
    void $ single '.'
    ext <- string "jpg" <|> string "jpeg" <|> string "png"
    pure . Image . URL $ path <> "." <> ext

link :: Parser Words
link = between (single '[') (single ']') $ Link
  <$> between (single '[') (single ']') (URL <$> someTill ']')
  <*> optional (between (single '[') (single ']') (someTill ']'))

-- meta :: Parser Org
-- meta = undefined

someTillEnd :: Parser Text
someTillEnd = someTill '\n'

manyTillEnd :: Parser Text
manyTillEnd = takeWhileP (Just "Many until the end of the line") (/= '\n')

someTill :: Char -> Parser Text
someTill c = takeWhile1P (Just "Some until the end of the line") (/= c)

--------------------------------------------------------------------------------
-- Pretty Printing

prettyOrgs :: [Org] -> Text
prettyOrgs = T.intercalate "\n\n" . map prettyOrg

prettyOrg :: Org -> Text
prettyOrg o = case o of
  Heading n ws -> T.unwords $ T.replicate n "*" : NEL.toList (NEL.map prettyWords ws)
  Code l t -> "#+begin_src" <> maybe "" (\(Language l') -> " " <> l' <> "\n") l
    <> t <> "\n"
    <> "#+end_src"
  Quote t -> "#+begin_quote\n" <> t <> "\n" <> "#+end_quote"
  Example t -> "#+begin_example\n" <> t <> "\n" <> "#+end_example"
  Paragraph ht -> par ht
  List items -> T.intercalate "\n" . map (("- " <>) . par) $ NEL.toList items
  where
    par :: NonEmpty Words -> Text
    par (h :| t) = prettyWords h <> foldMap para t

    -- | Stick punctuation directly behind the chars in front of it.
    para :: Words -> Text
    para b = case b of
      Punctuation _ -> prettyWords b
      _             -> " " <> prettyWords b

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
  Punctuation c           -> T.singleton c
  Plain t                 -> t
