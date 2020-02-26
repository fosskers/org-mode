{-# LANGUAGE DerivingStrategies #-}

module Data.Org
  ( -- * Types
    OrgFile(..)
  , Meta(..)
  , Org(..)
  , Words(..)
  , Item(..)
  , Row(..)
  , Column(..)
  , URL(..)
  , Language(..)
    -- * Parser
  , orgFile
    -- ** Internal Parsers
    -- | These are exposed for testing purposes.
  , meta
  , org
  , paragraph
  , table
  , list
  , line
    -- * Pretty Printing
  , prettyOrgFile
  , prettyOrgs
  , prettyOrg
  , prettyWords
  ) where

import           Control.Applicative.Combinators.NonEmpty hiding (someTill)
import           Control.Monad (void)
import           Data.Functor (($>))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Semigroup (sconcat)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorian, toGregorian)
import           Data.Void (Void)
import           Text.Megaparsec hiding (sepBy1, sepEndBy1, some, someTill)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf (printf)

--------------------------------------------------------------------------------
-- Types

-- | A complete @.org@ file with metadata.
data OrgFile = OrgFile
  { orgMeta    :: Meta
  , orgContent :: [Org] }
  deriving (Eq, Show)

data Meta = Meta
  { metaTitle    :: Maybe Text
  , metaDate     :: Maybe Day
  , metaAuthor   :: Maybe Text
  , metaHtmlHead :: Maybe Text }
  deriving (Eq, Show)

-- | Various sections of an org-mode file.
data Org
  = Heading Int (NonEmpty Words)
  | Quote Text
  | Example Text
  | Code (Maybe Language) Text
  | List (NonEmpty Item)
  | Table (NonEmpty Row)
  | Paragraph (NonEmpty Words)
  deriving stock (Eq, Show)

-- | A line in a bullet-list.
data Item = Item Int (NonEmpty Words) deriving (Eq, Show)

data Row = Break | Row (NonEmpty Column) deriving (Eq, Show)

data Column = Empty | Column (NonEmpty Words) deriving (Eq, Show)

data Words
  = Bold Text
  | Italic Text
  | Highlight Text
  | Underline Text
  | Verbatim Text
  | Strike Text
  | Link URL (Maybe Text)
  | Image URL
  | Punct Char
  | Plain Text
  deriving stock (Eq, Show)

-- | The url portion of a link.
newtype URL = URL Text deriving stock (Eq, Show)

-- | The programming language some source code block was written in.
newtype Language = Language Text deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void Text

orgFile :: Parser OrgFile
orgFile = space *> L.lexeme space (OrgFile <$> meta <*> org) <* eof

meta :: Parser Meta
meta = L.lexeme space $ Meta
  <$> optional (string "#+TITLE: "     *> someTillEnd <* space)
  <*> optional (string "#+DATE: "      *> date        <* space)
  <*> optional (string "#+AUTHOR: "    *> someTillEnd <* space)
  <*> optional (string "#+HTML_HEAD: " *> someTillEnd <* space)
  where
    date :: Parser Day
    date = fromGregorian
      <$> (L.decimal <* char '-')
      <*> (L.decimal <* char '-')
      <*> L.decimal

org :: Parser [Org]
org = L.lexeme space $ many block
  where
    block :: Parser Org
    block = choice
      [ try heading
      , try code
      , try example
      , try quote
      , try list
      , try table
      , paragraph ]

heading :: Parser Org
heading = L.lexeme space $ do
  stars <- some $ single '*'
  void $ single ' '
  Heading (NEL.length stars) <$> line '\n'

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
list = L.lexeme space $ List <$> sepEndBy1 item newline

-- | Conditions for ending the current bullet:
--
-- 1. You find two '\n' at the end of a line.
-- 2. The first two non-space characters of the next line are "- ".
item :: Parser Item
item = do
  leading <- takeWhileP (Just "space") (== ' ')
  void $ string "- "
  l <- bullet
  pure $ Item (T.length leading `div` 2) l
  where
    bullet :: Parser (NonEmpty Words)
    bullet = do
      l <- line '\n'
      try (lookAhead keepGoing *> space *> ((l <>) <$> bullet)) <|> pure l

    keepGoing :: Parser ()
    keepGoing = void $ char '\n' *> manyOf ' ' *> noneOf ['-', '\n']

table :: Parser Org
table = L.lexeme space $ Table <$> sepEndBy1 row (single '\n')
  where
    row :: Parser Row
    row = do
      void $ single '|'
      brk <|> (Row <$> sepEndBy1 column (single '|'))

    -- | If the line starts with @|-@, assume its a break regardless of what
    -- chars come after that.
    brk :: Parser Row
    brk = single '-' *> manyTillEnd $> Break

    column :: Parser Column
    column = do
      void $ someOf ' '
      (lookAhead (single '|') $> Empty) <|> (Column <$> line '|')

paragraph :: Parser Org
paragraph = L.lexeme space $ Paragraph . sconcat <$> sepEndBy1 (line '\n') newline

line :: Char -> Parser (NonEmpty Words)
line end = sepEndBy1 (wordChunk end) (manyOf ' ')

-- | RULES
--
-- 1. In-lined markup is not recognized: This is not*bold*. Neither is *this*here.
-- 2. Punctuation immediately after markup close /is/ allowed: *This*, in fact, is bold.
-- 3. Otherwise, a space, newline or EOF is necessary after the close.
-- 4. Any char after a link is fine.
-- 5. When rerendering, a space must not appear between the end of a markup close and
--    a punctuation/newline character.
-- 6. But any other character must have a space before it.
wordChunk :: Char -> Parser Words
wordChunk end = choice
  [ try $ Bold      <$> between (single '*') (single '*') (someTill '*') <* pOrS
  , try $ Italic    <$> between (single '/') (single '/') (someTill '/') <* pOrS
  , try $ Highlight <$> between (single '~') (single '~') (someTill '~') <* pOrS
  , try $ Verbatim  <$> between (single '=') (single '=') (someTill '=') <* pOrS
  , try $ Underline <$> between (single '_') (single '_') (someTill '_') <* pOrS
  , try $ Strike    <$> between (single '+') (single '+') (someTill '+') <* pOrS
  , try image
  , link
  , try $ Punct     <$> oneOf punc
  , Plain           <$> takeWhile1P (Just "plain text") (\c -> c /= ' ' && c /= end) ]
  where
    pOrS :: Parser ()
    pOrS = lookAhead $ void (oneOf $ end : ' ' : punc) <|> eof

punc :: String
punc = ".,!?():;'"

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

someTillEnd :: Parser Text
someTillEnd = someTill '\n'

manyTillEnd :: Parser Text
manyTillEnd = takeWhileP (Just "many until the end of the line") (/= '\n')

someTill :: Char -> Parser Text
someTill c = takeWhile1P (Just $ "some until " <> [c]) (/= c)

-- | Fast version of `some` specialized to `Text`.
someOf :: Char -> Parser Text
someOf c = takeWhile1P (Just $ "some of " <> [c]) (== c)

manyOf :: Char -> Parser Text
manyOf c = takeWhileP (Just $ "many of " <> [c]) (== c)

--------------------------------------------------------------------------------
-- Pretty Printing

prettyOrgFile :: OrgFile -> Text
prettyOrgFile (OrgFile m os) = metas <> "\n\n" <> prettyOrgs os
  where
    metas = T.intercalate "\n" $
      maybe [] (\t -> ["#+TITLE: " <> t]) (metaTitle m)
      <> maybe [] (pure . T.pack . day) (metaDate m)
      <> maybe [] (\a -> ["#+AUTHOR: " <> a]) (metaAuthor m)
      <> maybe [] (\h -> ["#+HTML_HEAD: " <> h]) (metaHtmlHead m)

    day :: Day -> String
    day d = case toGregorian d of
      (yr, mn, dy) -> printf "#+DATE: %d-%02d-%02d" yr mn dy

prettyOrgs :: [Org] -> Text
prettyOrgs = T.intercalate "\n\n" . map prettyOrg

prettyOrg :: Org -> Text
prettyOrg o = case o of
  Heading n ws -> T.unwords $ T.replicate n "*" : NEL.toList (NEL.map prettyWords ws)
  Code l t -> "#+begin_src" <> maybe "" (\(Language l') -> " " <> l' <> "\n") l
    <> t
    <> "\n#+end_src"
  Quote t -> "#+begin_quote\n" <> t <> "\n#+end_quote"
  Example t -> "#+begin_example\n" <> t <> "\n#+end_example"
  Paragraph ht -> par ht
  List items -> T.intercalate "\n" . map f $ NEL.toList items
  Table rows -> T.intercalate "\n" . map row $ NEL.toList rows
  where
    f :: Item -> Text
    f (Item i ws) = T.replicate (i * 2) " " <> "- " <> par ws

    par :: NonEmpty Words -> Text
    par (h :| t) = prettyWords h <> para h t

    -- | Stick punctuation directly behind the chars in front of it, while
    -- paying special attention to parentheses.
    para :: Words -> [Words] -> Text
    para _ []      = ""
    para pr (w:ws) = case pr of
      Punct '(' -> prettyWords w <> para w ws
      _ -> case w of
        Punct '(' -> " " <> prettyWords w <> para w ws
        Punct _   -> prettyWords w <> para w ws
        _         -> " " <> prettyWords w <> para w ws

    row :: Row -> Text
    row Break    = "|-|"
    row (Row cs) = "| " <> (T.intercalate " | " . map col $ NEL.toList cs) <> " |"

    col :: Column -> Text
    col Empty       = ""
    col (Column ws) = T.unwords . map prettyWords $ NEL.toList ws

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
  Punct c                 -> T.singleton c
  Plain t                 -> t
