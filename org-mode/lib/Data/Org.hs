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
    -- ** Top-level
    OrgFile(..)
  , emptyOrgFile
  , OrgDoc(..)
  , emptyDoc
  , allDocTags
    -- ** Timestamps
  , OrgDateTime(..)
  , OrgTime(..)
  , Repeater(..)
  , TimeDirection(..)
  , Interval(..)
    -- ** Markup
  , Section(..)
  , titled
  , allSectionTags
  , Todo(..)
  , Priority(..)
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
  , properties
  , property
  , paragraph
  , table
  , list
  , line
  , timestamp
  , date
  , timeRange
  , repeater
    -- * Pretty Printing
  , prettyOrgFile
  , prettyOrg
  , prettyWords
  ) where

import           Control.Applicative.Combinators.NonEmpty
import           Control.Monad (void, when)
import           Data.Bool (bool)
import           Data.Functor (($>))
import           Data.Hashable (Hashable)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Semigroup (sconcat)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, TimeOfDay(..), fromGregorian, showGregorian)
import           Data.Time.Calendar (DayOfWeek(..))
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           System.FilePath (takeExtension)
import           Text.Megaparsec hiding (sepBy1, sepEndBy1, some, someTill)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf (printf)

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
  deriving stock (Eq, Ord, Show, Generic)

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
  deriving stock (Eq, Ord, Show, Generic)

emptyDoc :: OrgDoc
emptyDoc = OrgDoc [] []

-- | All unique section tags in the entire document.
--
-- Section tags appear on the same row as a header title, but right-aligned.
--
-- @
-- * This is a Heading                :tag1:tag2:
-- @
allDocTags :: OrgDoc -> S.Set Text
allDocTags = foldMap allSectionTags . docSections

-- | Some logically distinct block of Org content.
data Block
  = Quote Text
  | Example Text
  | Code (Maybe Language) Text
  | List ListItems
  | Table (NonEmpty Row)
  | Paragraph (NonEmpty Words)
  deriving stock (Eq, Ord, Show, Generic)

-- | An org-mode timestamp. Must contain at least a year-month-day and the day
-- of the week:
--
-- @
-- \<2021-04-27 Tue\>
-- @
--
-- but also may contain a time:
--
-- @
-- \<2021-04-27 Tue 12:00\>
-- @
--
-- or a time range:
--
-- @
-- \<2021-04-27 Tue 12:00-13:00\>
-- @
--
-- and/or a repeater value:
--
-- @
-- \<2021-04-27 Tue +1w\>
-- @
data OrgDateTime = OrgDateTime
  { dateDay       :: Day
  , dateDayOfWeek :: DayOfWeek
  , dateTime      :: Maybe OrgTime
  , dateRepeat    :: Maybe Repeater }
  deriving stock (Eq, Show)

-- | A lack of a specific `OrgTime` is assumed to mean @00:00@, the earliest
-- possible time for that day.
instance Ord OrgDateTime where
  compare (OrgDateTime d0 _ mt0 _) (OrgDateTime d1 _ mt1 _) = case compare d0 d1 of
    LT -> LT
    GT -> GT
    EQ -> case (mt0, mt1) of
      (Nothing, Nothing) -> EQ
      (Just _, Nothing)  -> GT
      (Nothing, Just _)  -> LT
      (Just t0, Just t1) -> compare t0 t1

-- | The time portion of the full timestamp. May be a range, as seen in the
-- following full timestamp:
--
-- @
-- \<2021-04-27 Tue 12:00-13:00\>
-- @
data OrgTime = OrgTime
  { timeStart :: TimeOfDay
  , timeEnd   :: Maybe TimeOfDay }
  deriving stock (Eq, Ord, Show)

-- | An indication of how often a timestamp should be automatically reapplied in
-- the Org Agenda.
data Repeater = Repeater
  { repDirection :: TimeDirection
  , repValue     :: Word
  , repInterval  :: Interval }
  deriving stock (Eq, Ord, Show)

-- | Is the `Repeater` value an offset into the past or future?
data TimeDirection = Past | Future
  deriving stock (Eq, Ord, Show)

-- | The timestamp repitition unit.
data Interval = Day | Week | Month | Year
  deriving stock (Eq, Ord, Show)

-- | A subsection, marked by a heading line and followed recursively by an
-- `OrgDoc`.
--
-- @
-- * This is a Heading
--
-- This is content in the sub ~OrgDoc~.
-- @
data Section = Section
  { sectionTodo      :: Maybe Todo
  , sectionPriority  :: Maybe Priority
  , sectionHeading   :: NonEmpty Words
  , sectionTags      :: [Text]
  , sectionClosed    :: Maybe OrgDateTime
  , sectionDeadline  :: Maybe OrgDateTime
  , sectionScheduled :: Maybe OrgDateTime
  , sectionTimestamp :: Maybe OrgDateTime
    -- ^ A timestamp for general events that are neither a DEADLINE nor SCHEDULED.
  , sectionProps     :: M.Map Text Text
  , sectionDoc       :: OrgDoc }
  deriving stock (Eq, Ord, Show, Generic)

-- | A mostly empty invoking of a `Section`.
titled :: Words -> Section
titled ws = Section Nothing Nothing (ws:|[]) [] Nothing Nothing Nothing Nothing mempty emptyDoc

-- | All unique tags with a section and its subsections.
allSectionTags :: Section -> S.Set Text
allSectionTags (Section _ _ _ sts _ _ _ _ _ doc) = S.fromList sts <> allDocTags doc

-- | The completion state of a heading that is considered a "todo" item.
data Todo = TODO | DONE
  deriving stock (Eq, Ord, Show, Generic)

newtype Priority = Priority { priority :: Text }
  deriving stock (Eq, Ord, Show, Generic)

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
  deriving stock (Eq, Ord, Show, Generic)

-- | A line in a bullet-list. Can contain sublists, as shown in `ListItems`.
data Item = Item (NonEmpty Words) (Maybe ListItems)
  deriving stock (Eq, Ord, Show, Generic)

-- | A row in an org table. Can have content or be a horizontal rule.
--
-- @
-- | A | B | C |
-- |---+---+---|
-- | D | E | F |
-- @
data Row = Break | Row (NonEmpty Column)
  deriving stock (Eq, Ord, Show, Generic)

-- | A possibly empty column in an org table.
data Column = Empty | Column (NonEmpty Words)
  deriving stock (Eq, Ord, Show, Generic)

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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | The url portion of a link.
newtype URL = URL Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | The programming language some source code block was written in.
newtype Language = Language Text
  deriving stock (Eq, Ord, Show, Generic)

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
heading :: Parser (T.Text, Maybe Todo, Maybe Priority, NonEmpty Words, [Text])
heading = do
  stars <- someOf '*' <* char ' '
  (mtd, mpr, ws, mts) <- headerLine
  case mts of
    Nothing -> pure (stars, mtd, mpr, ws, [])
    Just ts -> pure (stars, mtd, mpr, ws, NEL.toList ts)

section :: Int -> Parser Section
section depth = L.lexeme space $ do
  (stars, td, pr, ws, ts) <- heading
  -- Fail if we've found a parent heading --
  when (T.length stars < depth) $ failure Nothing mempty
  -- Otherwise continue --
  (cl, dl, sc) <- fromMaybe (Nothing, Nothing, Nothing) <$> optional (try $ newline *> hspace *> timestamps)
  tm <- optional (try $ newline *> hspace *> stamp)
  props <- fromMaybe mempty <$> optional (try $ newline *> hspace *> properties)
  void space
  Section td pr ws ts cl dl sc tm props <$> orgP' (succ depth)

timestamps :: Parser (Maybe OrgDateTime, Maybe OrgDateTime, Maybe OrgDateTime)
timestamps = do
  mc <- optional closed
  void hspace
  md <- optional deadline
  void hspace
  ms <- optional scheduled
  case (mc, md, ms) of
    (Nothing, Nothing, Nothing) -> failure Nothing mempty
    _                           -> pure (mc, md, ms)

-- | An active timestamp.
stamp :: Parser OrgDateTime
stamp = between (char '<') (char '>') timestamp

closed :: Parser OrgDateTime
closed = string "CLOSED: " *> between (char '[') (char ']') timestamp

deadline :: Parser OrgDateTime
deadline = string "DEADLINE: " *> stamp

scheduled :: Parser OrgDateTime
scheduled = string "SCHEDULED: " *> stamp

timestamp :: Parser OrgDateTime
timestamp = OrgDateTime
  <$> date
  <*> (hspace1 *> dow)
  <*> optional (try $ hspace1 *> timeRange)
  <*> optional (hspace1 *> repeater)

date :: Parser Day
date = fromGregorian <$> decimal <*> (char '-' *> decimal) <*> (char '-' *> decimal)

dow :: Parser DayOfWeek
dow = choice
  [ Monday    <$ string "Mon"
  , Tuesday   <$ string "Tue"
  , Wednesday <$ string "Wed"
  , Thursday  <$ string "Thu"
  , Friday    <$ string "Fri"
  , Saturday  <$ string "Sat"
  , Sunday    <$ string "Sun" ]

timeRange :: Parser OrgTime
timeRange = OrgTime <$> t <*> optional (char '-' *> t)
  where
    t :: Parser TimeOfDay
    t = do
      h <- decimal
      void $ char ':'
      m <- decimal
      s <- optional $ do
        void $ char ':'
        decimal
      pure $ TimeOfDay h m (fromMaybe 0 s)

repeater :: Parser Repeater
repeater = Repeater
  <$> choice [ char '-' $> Past, char '+' $> Future ]
  <*> decimal
  <*> choice [ char 'd' $> Day, char 'w' $> Week, char 'm' $> Month, char 'y' $> Year ]

properties :: Parser (M.Map Text Text)
properties = do
  void $ string ":PROPERTIES:"
  void newline
  void hspace
  ps <- (hspace *> property <* newline <* hspace) `manyTill` string ":END:"
  pure $ M.fromList ps

property :: Parser (Text, Text)
property = do
  void $ char ':'
  key <- someTill' ':' -- TODO Newlines?
  void $ char ':'
  void hspace
  val <- takeWhile1P (Just "Property Value") (/= '\n')
  pure (key, val)

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

headerLine :: Parser (Maybe Todo, Maybe Priority, NonEmpty Words, Maybe (NonEmpty Text))
headerLine = do
  td <- optional . try $ (string "TODO" $> TODO) <|> (string "DONE" $> DONE)
  void hspace
  pr <- optional . try . fmap Priority $ between (char '[') (char ']') (char '#' *> someTill' ']')
  void hspace
  ws <- (wordChunk '\n' <* hspace) `someTill` lookAhead (try $ void tags <|> void (char '\n') <|> eof)
  ts <- optional tags
  pure (td, pr, ws, ts)

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
prettySection depth (Section td pr ws ts cl dl sc tm ps od) =
  T.intercalate "\n" $ catMaybes
  [ Just headig
  , stamps
  , time <$> tm
  , props
  , Just subdoc ]
  where
    pr' :: Priority -> Text
    pr' (Priority t) = "[#" <> t <> "]"

    -- TODO There is likely a punctuation bug here.
    --
    -- Sun Apr 25 09:59:01 AM PDT 2021: I wish you had elaborated.
    headig = T.unwords
      $ T.replicate depth "*"
      : catMaybes [ T.pack . show <$> td, pr' <$> pr ]
      <> NEL.toList (NEL.map prettyWords ws)
      <> bool [":" <> T.intercalate ":" ts <> ":"] [] (null ts)

    indent :: Text
    indent = T.replicate (depth + 1) " "

    -- | The order of "special" timestamps is CLOSED, DEADLINE, then SCHEDULED.
    -- Any permutation of these may appear.
    stamps :: Maybe Text
    stamps = case catMaybes [fmap cl' cl, fmap dl' dl, fmap sc' sc] of
      [] -> Nothing
      xs -> Just $ indent <> T.unwords xs

    cl' :: OrgDateTime -> Text
    cl' x = "CLOSED: [" <> prettyDateTime x <> "]"

    dl' :: OrgDateTime -> Text
    dl' x = "DEADLINE: <" <> prettyDateTime x <> ">"

    sc' :: OrgDateTime -> Text
    sc' x = "SCHEDULED: " <> time x

    time :: OrgDateTime -> Text
    time x = "<" <> prettyDateTime x <> ">"

    props :: Maybe Text
    props
      | null ps = Nothing
      | otherwise = Just . T.intercalate "\n" $ (indent <> ":PROPERTIES:") : items <> [indent <> ":END:"]
      where
        items :: [Text]
        items = map (\(k, v) -> indent <> ":" <> k <> ": " <> v) $ M.toList ps

    subdoc :: Text
    subdoc = prettyOrg' (succ depth) od

prettyDateTime :: OrgDateTime -> Text
prettyDateTime (OrgDateTime d w t r) =
  T.unwords $ catMaybes [ Just d', Just w', fmap prettyTime t, fmap prettyRepeat r ]
  where
    d' :: Text
    d' = T.pack $ showGregorian d

    w' :: Text
    w' = T.pack . take 3 $ show w

prettyTime :: OrgTime -> Text
prettyTime (OrgTime s me) = tod s <> maybe "" (\e -> "-" <> tod e) me
  where
    tod :: TimeOfDay -> Text
    tod (TimeOfDay h m _) = T.pack $ printf "%02d:%02d" h m

prettyRepeat :: Repeater -> Text
prettyRepeat (Repeater d v i) = d' <> T.pack (show v) <> i'
  where
    d' :: Text
    d' = case d of
      Past   -> "-"
      Future -> "+"

    i' :: Text
    i' = case i of
      Day   -> "d"
      Week  -> "w"
      Month -> "m"
      Year  -> "y"

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
