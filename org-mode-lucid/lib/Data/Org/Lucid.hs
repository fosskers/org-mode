{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Lucid
  ( -- * HTML Generation
    OrgStyle(..)
  , defaultStyle
  , html
  , body
  ) where

import           Control.Monad (void)
import           Data.Foldable (fold, traverse_)
import           Data.Functor (($>))
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Org
import qualified Data.Text as T
import           Lucid

-- TODO Give these all a `ToHtml a` instance.

--------------------------------------------------------------------------------
-- HTML Generation

-- | Rendering options.
data OrgStyle = OrgStyle
  { includeTitle     :: Bool
    -- ^ Whether to include the @#+TITLE: ...@ value as an @<h1>@ tag at the top
    -- of the document.
  , tableOfContents  :: Maybe Word
    -- ^ Optionally include a Table of Contents after the title. The displayed
    -- depth is configurable.
  , numberedHeadings :: Bool
    -- ^ Whether to number each heading.
  }

-- | Include the title and TOC, and number all headings. This mirrors the
-- behaviour of Emacs' native HTML export functionality.
defaultStyle :: OrgStyle
defaultStyle = OrgStyle True (Just 3) True

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
  maybe (pure ()) (h1_ . toHtml) $ metaTitle m
  -- toc o
  traverse_ orgHTML os

toc :: OrgFile -> Html ()
toc = undefined

orgHTML :: Org -> Html ()
orgHTML o = case o of
  Heading n ws -> heading n $ lineHTML ws
  Quote t -> blockquote_ . p_ $ toHtml t
  Example t -> pre_ [class_ "example"] $ toHtml t
  Code l t -> div_ [class_ "org-src-container"]
    $ pre_ [classes_ $ "src" : maybe [] (\(Language l') -> ["src-" <> l']) l]
    $ toHtml t
  List is -> listHTML is
  Table rw -> tableHTML rw
  Paragraph ws -> paragraphHTML ws

paragraphHTML :: NonEmpty Words -> Html ()
paragraphHTML (h :| t) = wordsHTML h <> foldMap para t
  where
    para :: Words -> Html ()
    para b = case b of
      Punct _ -> wordsHTML b
      _       -> " " <> wordsHTML b

-- | Render a grouping of `Words` that you expect to appear on a single line.
lineHTML :: NonEmpty Words -> Html ()
lineHTML = fold . intersperse " " . map wordsHTML . NEL.toList

listHTML :: NonEmpty Item -> Html ()
listHTML = void . work . sameIndent
  where
    work :: [NonEmpty Item] -> Html [NonEmpty Item]
    work [] = pure []
    work is = ul_ [class_ "org-ul"] $ loop is

    loop :: [NonEmpty Item] -> Html [NonEmpty Item]
    loop [] = pure []
    loop [is] = traverse_ f is $> []
    loop (is:r@(a:_)) = do
      let !curr = ind $ NEL.head is
      traverse_ f is
      if ind (NEL.head a) < curr
        then pure r
        else li_ (work r) >>= \case
          [] -> pure []
          next@(b:_) | ind (NEL.head b) == curr -> loop next
                     | otherwise -> pure next

    f :: Item -> Html ()
    f (Item _ ws) = li_ $ lineHTML ws

    ind :: Item -> Int
    ind (Item n _) = n

    sameIndent :: NonEmpty Item -> [NonEmpty Item]
    sameIndent = NEL.groupWith ind

tableHTML :: NonEmpty Row -> Html ()
tableHTML rs = table_ $ do
  thead_ toprow
  tbody_ $ traverse_ f rest
  where
    toprow = tr_ $ maybe (pure ()) (traverse_ g) h
    (h, rest) = j $ NEL.toList rs

    -- | Restructure the input such that the first `Row` is not a `Break`.
    j :: [Row] -> (Maybe (NonEmpty Column), [Row])
    j []           = (Nothing, [])
    j (Break : r)  = j r
    j (Row cs : r) = (Just cs, r)

    f :: Row -> Html ()
    f Break    = pure ()
    f (Row cs) = tr_ $ traverse_ g cs

    g :: Column -> Html ()
    g Empty       = th_ ""
    g (Column ws) = th_ $ lineHTML ws

heading :: Int -> (Html () -> Html ())
heading n = case n of
  1 -> h2_
  2 -> h3_
  3 -> h4_
  4 -> h5_
  5 -> h6_
  _ -> id

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
