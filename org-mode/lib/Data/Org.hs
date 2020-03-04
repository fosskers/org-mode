{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Org
  ( -- * Types
    OrgFile(..)
  , emptyOrgFile
  , Meta(..)
  , emptyMeta
  , OrgDoc(..)
  , emptyDoc
  , Section(..)
  , Block(..)
  , Words(..)
  , ListItems(..)
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
  , section
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

import           Control.Applicative.Combinators.NonEmpty
import           Control.Monad (void, when)
import           Data.Functor (($>))
import           Data.Hashable (Hashable(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Semigroup (sconcat)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorian, toGregorian)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           System.FilePath (takeExtension)
import           Text.Megaparsec hiding (sepBy1, sepEndBy1, some, someTill)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf (printf)

--------------------------------------------------------------------------------
-- Types

-- | A complete @.org@ file with metadata.
data OrgFile = OrgFile
  { orgMeta :: Meta
  , orgDoc  :: OrgDoc }
  deriving stock (Eq, Show, Generic)

emptyOrgFile :: OrgFile
emptyOrgFile = OrgFile emptyMeta emptyDoc

data Meta = Meta
  { metaTitle    :: Maybe Text
  , metaDate     :: Maybe Day
  , metaAuthor   :: Maybe Text
  , metaHtmlHead :: Maybe Text
  , metaOptions  :: Maybe Text }
  deriving stock (Eq, Show, Generic)

emptyMeta :: Meta
emptyMeta = Meta Nothing Nothing Nothing Nothing Nothing

-- | A recursive Org document. These are zero or more blocks of markup, followed
-- by zero or more subsections.
data OrgDoc = OrgDoc
  { docBlocks   :: [Block]
  , docSections :: [Section] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

emptyDoc :: OrgDoc
emptyDoc = OrgDoc [] []

-- | Some logically distinct block of Org content.
data Block
  = Quote Text
  | Example Text
  | Code (Maybe Language) Text
  | List ListItems
  | Table (NonEmpty Row)
  | Paragraph (NonEmpty Words)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A subsection, marked by a heading line and followed recursively by an
-- `OrgDoc`.
data Section = Section
  { sectionHeading :: NonEmpty Words
  , sectionDoc     :: OrgDoc }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

newtype ListItems = ListItems (NonEmpty Item)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A line in a bullet-list.
data Item = Item (NonEmpty Words) (Maybe ListItems)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Row = Break | Row (NonEmpty Column)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data Column = Empty | Column (NonEmpty Words)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | The url portion of a link.
newtype URL = URL Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | The programming language some source code block was written in.
newtype Language = Language Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

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
  <*> optional (string "#+OPTIONS: "   *> someTillEnd <* space)
  where
    date :: Parser Day
    date = fromGregorian
      <$> (L.decimal <* char '-')
      <*> (L.decimal <* char '-')
      <*> L.decimal

org :: Parser OrgDoc
org = L.lexeme space $ OrgDoc
  <$> many block
  <*> many section
  where
    block :: Parser Block
    block = choice
      [ try code
      , try example
      , try quote
      , try list
      , try table
      , paragraph ]

-- TODO Naive and not correct. This will make all headings subordinate to the
-- one previous.
section :: Parser Section
section = L.lexeme space $ do
  undefined
  -- void . some $ char '*'
  -- void $ char ' '
  -- Heading <$> line '\n' <*> (space *> org)

quote :: Parser Block
quote = L.lexeme space $ do
  void top <* newline
  ls <- manyTill (manyTillEnd <* newline) bot
  pure . Quote $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_QUOTE" <|> string "begin_quote")
    bot = string "#+" *> (string "END_QUOTE" <|> string "end_quote")

example :: Parser Block
example = L.lexeme space $ do
  void top <* newline
  ls <- manyTill (manyTillEnd <* newline) bot
  pure . Example $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_EXAMPLE" <|> string "begin_example")
    bot = string "#+" *> (string "END_EXAMPLE" <|> string "end_example")

code :: Parser Block
code = L.lexeme space $ do
  lang <- top *> optional lng <* newline
  ls <- manyTill (manyTillEnd <* newline) bot
  pure . Code (Language <$> lang) $ T.intercalate "\n" ls
  where
    top = string "#+" *> (string "BEGIN_SRC" <|> string "begin_src")
    bot = string "#+" *> (string "END_SRC" <|> string "end_src")
    lng = char ' '  *> someTillEnd

list :: Parser Block
list = L.lexeme space $ List <$> listItems 0

listItems :: Int -> Parser ListItems
listItems indent = ListItems
  <$> sepBy1 (item indent) (try $ newline *> lookAhead (nextItem indent))

nextItem :: Int -> Parser ()
nextItem indent = do
  void . string $ T.replicate indent " "
  void $ string "- "

-- | Conditions for ending the current bullet:
--
-- 1. You find two '\n' at the end of a line.
-- 2. The first two non-space characters of the next line are "- ".
item :: Int -> Parser Item
item indent = do
  leading <- string $ T.replicate indent " "
  void $ string "- "
  l <- bullet
  let !nextInd = T.length leading + 2
  Item l <$> optional (try $ newline *> listItems nextInd)
  where
    bullet :: Parser (NonEmpty Words)
    bullet = do
      l <- line '\n'
      try (lookAhead keepGoing *> space *> ((l <>) <$> bullet)) <|> pure l

    keepGoing :: Parser ()
    keepGoing = void $ char '\n' *> manyOf ' ' *> noneOf ['-', '\n']

table :: Parser Block
table = L.lexeme space $ Table <$> sepEndBy1 row (char '\n')
  where
    row :: Parser Row
    row = do
      void $ char '|'
      brk <|> (Row <$> sepEndBy1 column (char '|'))

    -- | If the line starts with @|-@, assume its a break regardless of what
    -- chars come after that.
    brk :: Parser Row
    brk = char '-' *> manyTillEnd $> Break

    column :: Parser Column
    column = do
      void $ someOf ' '
      (lookAhead (char '|') $> Empty) <|> (Column <$> line '|')

paragraph :: Parser Block
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
  [ try $ Bold      <$> between (char '*') (char '*') (someTill' '*') <* pOrS
  , try $ Italic    <$> between (char '/') (char '/') (someTill' '/') <* pOrS
  , try $ Highlight <$> between (char '~') (char '~') (someTill' '~') <* pOrS
  , try $ Verbatim  <$> between (char '=') (char '=') (someTill' '=') <* pOrS
  , try $ Underline <$> between (char '_') (char '_') (someTill' '_') <* pOrS
  , try $ Strike    <$> between (char '+') (char '+') (someTill' '+') <* pOrS
  , try image
  , try link
  , try $ Punct     <$> oneOf punc
  , Plain           <$> takeWhile1P (Just "plain text") (\c -> c /= ' ' && c /= end) ]
  where
    pOrS :: Parser ()
    pOrS = lookAhead $ void (oneOf $ end : ' ' : punc) <|> eof

punc :: String
punc = ".,!?():;'"

image :: Parser Words
image = between (char '[') (char ']') $
  between (char '[') (char ']') $ do
    path <- someTill' ']'
    let !ext = takeExtension $ T.unpack path
    when (ext `notElem` [".jpg", ".jpeg", ".png"]) $ failure Nothing mempty
    pure . Image $ URL path

link :: Parser Words
link = between (char '[') (char ']') $ Link
  <$> between (char '[') (char ']') (URL <$> someTill' ']')
  <*> optional (between (char '[') (char ']') (someTill' ']'))

someTillEnd :: Parser Text
someTillEnd = someTill' '\n'

manyTillEnd :: Parser Text
manyTillEnd = takeWhileP (Just "many until the end of the line") (/= '\n')

someTill' :: Char -> Parser Text
someTill' c = takeWhile1P (Just $ "some until " <> [c]) (/= c)

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

prettyOrgs :: OrgDoc -> Text
prettyOrgs = undefined -- T.intercalate "\n\n" . map prettyOrg

prettyOrg :: OrgDoc -> Text
prettyOrg = undefined -- prettyOrg' 1

prettyBlock :: Int -> Block -> Text
prettyBlock depth o = case o of
  -- Heading ws os -> T.intercalate "\n"
  --   $ T.unwords (T.replicate depth "*" : NEL.toList (NEL.map prettyWords ws))
  --   : map (prettyOrg' (succ depth)) os
  Code l t -> "#+begin_src" <> maybe "" (\(Language l') -> " " <> l' <> "\n") l
    <> t
    <> "\n#+end_src"
  Quote t -> "#+begin_quote\n" <> t <> "\n#+end_quote"
  Example t -> "#+begin_example\n" <> t <> "\n#+end_example"
  Paragraph ht -> par ht
  List items -> lis 0 items
  Table rows -> T.intercalate "\n" . map row $ NEL.toList rows
  where
    lis :: Int -> ListItems -> Text
    lis indent (ListItems is) = T.intercalate "\n" . map (f indent) $ NEL.toList is

    f :: Int -> Item -> Text
    f indent (Item ws li) =
      T.replicate indent " " <> "- " <> par ws
      <> maybe "" (\is -> "\n" <> lis (indent + 2) is) li

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
