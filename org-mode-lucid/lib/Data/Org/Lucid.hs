{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Data.Org.Lucid
-- Copyright : (c) Colin Woodbury, 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- This library converts `OrgFile` values into `Html` structures from the Lucid
-- library. This allows one to generate valid, standalone HTML pages from an Org
-- file, but also to inject that HTML into a preexisting Lucid `Html` structure,
-- such as a certain section of a web page.

module Data.Org.Lucid
  ( -- * HTML Conversion
    -- | Consider `defaultStyle` as the style to pass to these functions.
    html
  , body
    -- * Styling
  , OrgStyle(..)
  , defaultStyle
  , TOC(..)
  , Highlighting
  , codeHTML
  ) where

import           Control.Monad (when)
import           Data.Foldable (traverse_)
import           Data.Hashable (hash)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import           Data.Org
import qualified Data.Text as T
import           Lucid
import           Text.Printf (printf)

--------------------------------------------------------------------------------
-- HTML Generation

-- | Rendering options.
data OrgStyle = OrgStyle
  { includeTitle    :: Bool
    -- ^ Whether to include the @#+TITLE: ...@ value as an @<h1>@ tag at the top
    -- of the document.
  , tableOfContents :: Maybe TOC
    -- ^ Optionally include a Table of Contents after the title. The displayed
    -- depth is configurable.
  , bootstrap       :: Bool
    -- ^ Whether to add Twitter Bootstrap classes to certain elements.
  , highlighting    :: Highlighting
    -- ^ A function to give @\<code\>@ blocks syntax highlighting.
  , separator       :: Maybe Char
    -- ^ `Char` to insert between elements during rendering, for example having
    -- a space between words. Asian languages, for instance, might want this to
    -- be `Nothing`.
  }

-- | Options for rendering a Table of Contents in the document.
data TOC = TOC
  { tocTitle :: T.Text
    -- ^ The text of the TOC to be rendered in an @<h2>@ element.
  , tocDepth :: Word
    -- ^ The number of levels to give the TOC.
  }

-- | A function to give @\<code\>@ blocks syntax highlighting.
type Highlighting = Maybe Language -> T.Text -> Html ()

-- | Include the title and 3-level TOC named @Table of Contents@, and don't
-- include Twitter Bootstrap classes. This mirrors the behaviour of Emacs'
-- native HTML export functionality.
defaultStyle :: OrgStyle
defaultStyle = OrgStyle True (Just $ TOC "Table of Contents" 3) False codeHTML (Just ' ')

-- | Convert a parsed `OrgFile` into a full HTML document readable in a browser.
html :: OrgStyle -> OrgFile -> Html ()
html os o@(OrgFile m _) = html_ $ do
  head_ $ title_ (maybe "" toHtml $ M.lookup "TITLE" m)
  body_ $ body os o

-- | Convert a parsed `OrgFile` into the body of an HTML document, so that it
-- could be injected into other Lucid `Html` structures.
--
-- Does __not__ wrap contents in a @\<body\>@ tag.
body :: OrgStyle -> OrgFile -> Html ()
body os (OrgFile m od) = do
  when (includeTitle os) . traverse_ (h1_ [class_ "title"] . toHtml) $ M.lookup "TITLE" m
  traverse_ (\t -> toc os t od) $ tableOfContents os
  orgHTML os od

-- | A unique identifier that can be used as an HTML @id@ attribute.
tocLabel :: NonEmpty Words -> T.Text
tocLabel = ("org" <>) . T.pack . take 6 . printf "%x" . hash

toc :: OrgStyle -> TOC -> OrgDoc -> Html ()
toc _ _ (OrgDoc _ []) = pure ()
toc os t od           = h2_ (toHtml $ tocTitle t) *> toc' os t 1 od

toc' :: OrgStyle -> TOC -> Word -> OrgDoc -> Html ()
toc' _ _ _ (OrgDoc _ []) = pure ()
toc' os t depth (OrgDoc _ ss)
  | depth > tocDepth t = pure ()
  | otherwise = ul_ $ traverse_ f ss
  where
    f :: Section -> Html ()
    f (Section ws _ od) = do
      li_ $ a_ [href_ $ "#" <> tocLabel ws] $ paragraphHTML os ws
      toc' os t (succ depth) od

orgHTML :: OrgStyle -> OrgDoc -> Html ()
orgHTML os = orgHTML' os 1

orgHTML' :: OrgStyle -> Int -> OrgDoc -> Html ()
orgHTML' os depth (OrgDoc bs ss) = do
  traverse_ (blockHTML os) bs
  traverse_ (sectionHTML os depth) ss

sectionHTML :: OrgStyle -> Int -> Section -> Html ()
sectionHTML os depth (Section ws _ od) = do
  heading [id_ $ tocLabel ws] $ paragraphHTML os ws
  orgHTML' os (succ depth) od
  where
    heading :: [Attribute] -> Html () -> Html ()
    heading as h = case depth of
      1 -> h2_ as h
      2 -> h3_ as h
      3 -> h4_ as h
      4 -> h5_ as h
      5 -> h6_ as h
      _ -> h

blockHTML :: OrgStyle -> Block -> Html ()
blockHTML os b = case b of
  Quote t      -> blockquote_ . p_ $ toHtml t
  Example t    -> pre_ [class_ "example"] $ toHtml t
  Code l t     -> highlighting os l t
  List is      -> listItemsHTML os is
  Table rw     -> tableHTML os rw
  Paragraph ws -> p_ $ paragraphHTML os ws

-- | Mimicks the functionality of Emacs' native HTML export.
codeHTML :: Highlighting
codeHTML l t = div_ [class_ "org-src-container"]
  $ pre_ [classes_ $ "src" : maybe [] (\(Language l') -> ["src-" <> l']) l]
  $ toHtml t

paragraphHTML :: OrgStyle -> NonEmpty Words -> Html ()
paragraphHTML os (h :| t) = wordsHTML h <> para h t
  where
    sep :: Html ()
    sep = maybe "" (toHtml . T.singleton) $ separator os

    para :: Words -> [Words] -> Html ()
    para _ [] = ""
    para pr (w:ws) = case pr of
      Punct '(' -> wordsHTML w <> para w ws
      _ -> case w of
        Punct '(' -> sep <> wordsHTML w <> para w ws
        Punct _   -> wordsHTML w <> para w ws
        _         -> sep <> wordsHTML w <> para w ws

listItemsHTML :: OrgStyle -> ListItems -> Html ()
listItemsHTML os (ListItems is) = ul_ [class_ "org-ul"] $ traverse_ f is
  where
    f :: Item -> Html ()
    f (Item ws next) = li_ $ paragraphHTML os ws >> traverse_ (listItemsHTML os) next

tableHTML :: OrgStyle -> NonEmpty Row -> Html ()
tableHTML os rs = table_ tblClasses $ do
  thead_ headClasses toprow
  tbody_ $ traverse_ f rest
  where
    tblClasses
      | bootstrap os = [classes_ ["table", "table-bordered", "table-hover"]]
      | otherwise = []

    headClasses
      | bootstrap os = [class_ "thead-dark"]
      | otherwise = []

    toprow = tr_ $ traverse_ (traverse_ g) h
    (h, rest) = j $ NEL.toList rs

    -- | Restructure the input such that the first `Row` is not a `Break`.
    j :: [Row] -> (Maybe (NonEmpty Column), [Row])
    j []           = (Nothing, [])
    j (Break : r)  = j r
    j (Row cs : r) = (Just cs, r)

    -- | Potentially render a `Row`.
    f :: Row -> Html ()
    f Break    = pure ()
    f (Row cs) = tr_ $ traverse_ k cs

    -- | Render a header row.
    g :: Column -> Html ()
    g Empty       = th_ [scope_ "col"] ""
    g (Column ws) = th_ [scope_ "col"] $ paragraphHTML os ws

    -- | Render a normal row.
    k :: Column -> Html ()
    k Empty       = td_ ""
    k (Column ws) = td_ $ paragraphHTML os ws

wordsHTML :: Words -> Html ()
wordsHTML ws = case ws of
  Bold t          -> b_ $ toHtml t
  Italic t        -> i_ $ toHtml t
  Highlight t     -> code_ [class_ "org-highlight"] $ toHtml t
  Underline t     -> span_ [style_ "text-decoration: underline;"] $ toHtml t
  Verbatim t      -> toHtml t
  Strike t        -> span_ [style_ "text-decoration: line-through;"] $ toHtml t
  Link (URL u) mt -> a_ [href_ u] $ maybe "" toHtml mt
  Image (URL u)   -> img_ [src_ u]
  Tags ts         -> toHtml $ ":" <> T.intercalate ":" (NEL.toList ts) <> ":"
  Punct c         -> toHtml $ T.singleton c
  Plain t         -> toHtml t
