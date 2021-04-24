{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module    : Data.Org
-- Copyright : (c) Colin Woodbury, 2020 - 2021
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- This library parses text in the <https://orgmode.org/ Emacs Org Mode> format.
--
-- Use the `org` function to parse a `T.Text` value.

module Data.Org
  ( -- * Types
    OrgFile(..)
  , emptyOrgFile
  , OrgDoc(..)
  , emptyDoc
  , allDocTags
  , Section(..)
  , allSectionTags
  , Block(..)
  , Words(..)
  , ListItems(..)
  , Item(..)
  , Row(..)
  , Column(..)
  , URL(..)
  , Language(..)
    -- * Parsing
  , org
    -- ** Internal Parsers
    -- | These are exposed for testing purposes.
  , orgFile
  , meta
  , orgP
  , section
  , paragraph
  , table
  , list
  , line
    -- * Pretty Printing
  , prettyOrgFile
  , prettyOrg
  , prettyWords
  ) where

import           Control.Applicative.Combinators.NonEmpty
import           Control.Monad (void, when)
import           Data.Bool (bool)
import           Data.Functor (($>))
import           Data.Hashable (Hashable(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import           Data.Semigroup (sconcat)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           System.FilePath (takeExtension)
import           Text.Megaparsec hiding (sepBy1, sepEndBy1, some, someTill)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- Types

-- | A complete @.org@ file with metadata.
data OrgFile = OrgFile
  { orgMeta :: M.Map Text Text
  -- ^ Top-level fields like:
  --
  -- @
  -- #+TITLE: Curing Cancer with Haskell
  -- #+DATE: 2020-02-25
  -- #+AUTHOR: Colin
  -- @
  , orgDoc  :: OrgDoc }
  deriving stock (Eq, Show, Generic)

emptyOrgFile :: OrgFile
emptyOrgFile = OrgFile mempty emptyDoc

-- | A recursive Org document. These are zero or more blocks of markup, followed
-- by zero or more subsections.
--
-- @
-- This is some top-level text.
--
-- * Important heading
--
-- ** Less important subheading
-- @
data OrgDoc = OrgDoc
  { docBlocks   :: [Block]
  , docSections :: [Section] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

emptyDoc :: OrgDoc
emptyDoc = OrgDoc [] []

-- | All unique section tags in the entire document.
--
-- Section tags appear on the same row as a header title, but right-aligned.
--
-- @
-- * This is a Heading                :tag1:tag2:
-- @
allDocTags :: OrgDoc -> Set Text
allDocTags = foldMap allSectionTags . docSections

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
--
-- @
-- * This is a Heading
--
-- This is content in the sub ~OrgDoc~.
-- @
data Section = Section
  { sectionHeading :: NonEmpty Words
  , sectionTags    :: [Text]
  , sectionDoc     :: OrgDoc }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | All unique tags with a section and its subsections.
allSectionTags :: Section -> Set Text
allSectionTags (Section _ sts doc) = S.fromList sts <> allDocTags doc

-- | An org list constructed of @-@ characters.
--
-- @
-- - Feed the cat
--   - The good stuff
-- - Feed the dog
--   - He'll eat anything
-- - Feed the bird
-- - Feed the alligator
-- - Feed the elephant
-- @
newtype ListItems = ListItems (NonEmpty Item)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A line in a bullet-list. Can contain sublists, as shown in `ListItems`.
data Item = Item (NonEmpty Words) (Maybe ListItems)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A row in an org table. Can have content or be a horizontal rule.
--
-- @
-- | A | B | C |
-- |---+---+---|
-- | D | E | F |
-- @
data Row = Break | Row (NonEmpty Column)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A possibly empty column in an org table.
data Column = Empty | Column (NonEmpty Words)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | The fundamental unit of Org text content. `Plain` units are split
-- word-by-word.
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

-- | Attempt to parse an `OrgFile`.
org :: Text -> Maybe OrgFile
org = parseMaybe orgFile

type Parser = Parsec Void Text

orgFile :: Parser OrgFile
orgFile = space *> L.lexeme space (OrgFile <$> meta <*> orgP) <* eof

meta :: Parser (M.Map Text Text)
meta = L.lexeme space $ M.fromList <$> keyword `sepEndBy` newline
  where
    keyword :: Parser (Text, Text)
    keyword = do
      void $ string "#+"
      key <- someTill' ':'
      void $ string ": "
      val <- someTillEnd
      pure (key, val)

orgP :: Parser OrgDoc
orgP = orgP' 1

orgP' :: Int -> Parser OrgDoc
orgP' depth = L.lexeme space $ OrgDoc
  <$> many block
  <*> many (try $ section depth)
  where
    block :: Parser Block
    block = choice
      [ try code
      , try example
      , try quote
      , try list
      , try table
      , paragraph ]  -- TODO Paragraph needs to fail if it detects a heading.

-- | If a line starts with @*@ and a space, it is a `Section` heading.
heading :: Parser (T.Text, NonEmpty Words, [Text])
heading = do
  stars <- someOf '*' <* char ' '
  (ws, mts) <- headerLine
  case mts of
    Nothing -> pure (stars, ws, [])
    Just ts -> pure (stars, ws, NEL.toList ts)

section :: Int -> Parser Section
section depth = L.lexeme space $ do
  (stars, ws, ts) <- heading
  -- Fail if we've found a parent heading --
  when (T.length stars < depth) $ failure Nothing mempty
  -- Otherwise continue --
  void space
  Section ws ts <$> orgP' (succ depth)

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
paragraph = L.lexeme space $ do
  notFollowedBy heading
  Paragraph . sconcat <$> sepEndBy1 (line '\n') newline

headerLine :: Parser (NonEmpty Words, Maybe (NonEmpty Text))
headerLine = do
  ws <- (wordChunk '\n' <* hspace) `someTill` lookAhead (void tags <|> void (char '\n') <|> eof)
  ts <- optional tags
  pure (ws, ts)

line :: Char -> Parser (NonEmpty Words)
line end = wordChunk end `sepEndBy1` manyOf ' '

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
    -- | Punctuation, space, or the end of the file.
    pOrS :: Parser ()
    pOrS = lookAhead $ void (oneOf $ end : ' ' : punc) <|> eof

punc :: String
punc = ".,!?():;'"

tags :: Parser (NonEmpty Text)
tags = do
  void $ char ':'
  (T.pack . NEL.toList <$> some (alphaNumChar <|> char '_' <|> char '@')) `sepEndBy1` char ':'

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
prettyOrgFile (OrgFile m os) = metas <> "\n\n" <> prettyOrg os
  where
    metas = T.intercalate "\n"
      $ map (\(l, t) -> "#+" <> l <> ": " <> t)
      $ M.toList m

prettyOrg :: OrgDoc -> Text
prettyOrg  = prettyOrg' 1

prettyOrg' :: Int -> OrgDoc -> Text
prettyOrg' depth (OrgDoc bs ss) =
  T.intercalate "\n\n" $ map prettyBlock bs <> map (prettySection depth) ss

prettySection :: Int -> Section -> Text
prettySection depth (Section ws ts od) = headig <> "\n\n" <> subdoc
  where
    -- TODO There is likely a punctuation bug here.
    headig = T.unwords
      $ T.replicate depth "*"
      : NEL.toList (NEL.map prettyWords ws)
      <> bool [":" <> T.intercalate ":" ts <> ":"] [] (null ts)

    subdoc :: Text
    subdoc = prettyOrg' (succ depth) od

prettyBlock :: Block -> Text
prettyBlock o = case o of
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
