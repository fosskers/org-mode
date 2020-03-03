{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Lucid
  ( -- * HTML Generation
    OrgStyle(..)
  , TOC(..)
  , defaultStyle
  , html
  , body
  ) where

import           Data.Foldable (fold, traverse_)
import           Data.Hashable (hash)
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Org
import qualified Data.Text as T
import           Lucid
import           Text.Printf (printf)

-- TODO Give these all a `ToHtml a` instance.

--------------------------------------------------------------------------------
-- HTML Generation

-- | Rendering options.
data OrgStyle = OrgStyle
  { includeTitle     :: Bool
    -- ^ Whether to include the @#+TITLE: ...@ value as an @<h1>@ tag at the top
    -- of the document.
  , tableOfContents  :: Maybe TOC
    -- ^ Optionally include a Table of Contents after the title. The displayed
    -- depth is configurable.
  , numberedHeadings :: Bool
    -- ^ Whether to number each heading.
  , bootstrap        :: Bool
    -- ^ Whether to add bootstrap classes to certain elements.
  }

-- | Options for rendering a Table of Contents in the document.
data TOC = TOC
  { tocTitle :: T.Text
    -- ^ The text of the TOC to be rendered in an @<h2>@ element.
  , tocDepth :: Word
    -- ^ How many levels to give the TOC.
  }

-- | Include the title and TOC, number all headings, and don't include Twitter
-- Bootstrap classes. This mirrors the behaviour of Emacs' native HTML export
-- functionality.
defaultStyle :: OrgStyle
defaultStyle = OrgStyle True (Just $ TOC "Table of Contents" 3) True False

-- | Convert a parsed `OrgFile` into a full HTML document readable in a browser.
html :: OrgFile -> Html ()
html o@(OrgFile m _) = html_ $ do
  head_ $ title_ (maybe "" toHtml $ metaTitle m)
  body_ $ body o

-- | Convert a parsed `OrgFile` into the body of an HTML document, so that it
-- could be injected into other Lucid `Html` structures.
--
-- Does not wrap contents in a @<body>@ tag.
body :: OrgFile -> Html ()
body o@(OrgFile m os) = do
  maybe (pure ()) (h1_ [class_ "title"] . toHtml) $ metaTitle m
  toc o
  traverse_ orgHTML os

-- | A unique identifier that can be used as an HTML @id@ attribute.
tocLabel :: NonEmpty Words -> T.Text
tocLabel ws = ("org" <>) . T.pack . take 6 . printf "%x" $ hash ws

headings :: [Org] -> [(Int, NonEmpty Words)]
headings = foldr f []
  where
    f :: Org -> [(Int, NonEmpty Words)] -> [(Int, NonEmpty Words)]
    f (Heading n ws) acc = (n, ws) : acc
    f _ acc              = acc

toc :: OrgFile -> Html ()
toc (OrgFile _ os) = case headings os of
  [] -> pure ()
  hs -> do
    h2_ "Table of Contents"
    ul_ $ traverse_ (\(_, ws) -> li_ $ a_ [href_ $ "#" <> tocLabel ws] $ lineHTML ws) hs

orgHTML :: Org -> Html ()
orgHTML o = case o of
  Heading n ws -> heading n [id_ $ tocLabel ws] $ lineHTML ws
  Quote t -> blockquote_ . p_ $ toHtml t
  Example t -> pre_ [class_ "example"] $ toHtml t
  Code l t -> div_ [class_ "org-src-container"]
    $ pre_ [classes_ $ "src" : maybe [] (\(Language l') -> ["src-" <> l']) l]
    $ toHtml t
  List is -> listItemsHTML is
  Table rw -> tableHTML rw
  Paragraph ws -> p_ $ paragraphHTML ws
  where
    heading :: Int -> [Attribute] -> Html () -> Html ()
    heading n as h = case n of
      1 -> h2_ as h
      2 -> h3_ as h
      3 -> h4_ as h
      4 -> h5_ as h
      5 -> h6_ as h
      _ -> h

paragraphHTML :: NonEmpty Words -> Html ()
paragraphHTML (h :| t) = wordsHTML h <> para h t
  where
    para :: Words -> [Words] -> Html ()
    para _ [] = ""
    para pr (w:ws) = case pr of
      Punct '(' -> wordsHTML w <> para w ws
      _ -> case w of
        Punct '(' -> " " <> wordsHTML w <> para w ws
        Punct _   -> wordsHTML w <> para w ws
        _         -> " " <> wordsHTML w <> para w ws

-- | Render a grouping of `Words` that you expect to appear on a single line.
lineHTML :: NonEmpty Words -> Html ()
lineHTML = fold . intersperse " " . map wordsHTML . NEL.toList

listItemsHTML :: ListItems -> Html ()
listItemsHTML (ListItems is) = ul_ [class_ "org-ul"] $ traverse_ f is
  where
    f :: Item -> Html ()
    f (Item ws next) = li_ $ lineHTML ws >> maybe (pure ()) listItemsHTML next

tableHTML :: NonEmpty Row -> Html ()
tableHTML rs = table_ [classes_ ["table", "table-bordered", "table-hover"]] $ do
  thead_ [class_ "thead-dark"] toprow
  tbody_ $ traverse_ f rest
  where
    toprow = tr_ $ maybe (pure ()) (traverse_ g) h
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
    g (Column ws) = th_ [scope_ "col"] $ lineHTML ws

    -- | Render a normal row.
    k :: Column -> Html ()
    k Empty       = td_ ""
    k (Column ws) = td_ $ lineHTML ws

wordsHTML :: Words -> Html ()
wordsHTML ws = case ws of
  Bold t          -> b_ $ toHtml t
  Italic t        -> i_ $ toHtml t
  Highlight t     -> code_ $ toHtml t
  Underline t     -> span_ [style_ "text-decoration: underline;"] $ toHtml t
  Verbatim t      -> toHtml t
  Strike t        -> span_ [style_ "text-decoration: line-through;"] $ toHtml t
  Link (URL u) mt -> a_ [href_ u] $ maybe "" toHtml mt
  Image (URL u)   -> img_ [src_ u]
  Punct c         -> toHtml $ T.singleton c
  Plain t         -> toHtml t
