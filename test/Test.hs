{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Data.Maybe (isJust)
import           Data.Org
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec
import           Text.Pretty.Simple

---

main :: IO ()
main = do
  simple <- T.readFile "test/simple.org"
  full   <- T.readFile "test/test.org"
  let fl = parseMaybe org full
  pPrintNoColor fl
  maybe (putStrLn "COULDN'T PARSE") T.putStrLn $ prettyOrgs <$> fl
  defaultMain $ suite simple full

suite :: T.Text -> T.Text -> TestTree
suite simple full = testGroup "Unit Tests"
  [ testGroup "Basic Markup"
    [ testCase "Header" $ parseMaybe org "* Header"
      @?= Just [Heading 1 [Plain "Header"]]
    , testCase "Headers" $ parseMaybe org "* Header\n** Subheader"
      @?= Just [Heading 1 [Plain "Header"], Heading 2 [Plain "Subheader"]]
    , testCase "Bold" $ parseMaybe org "*Bold*"
      @?= Just [Paragraph [Bold "Bold"]]
    , testCase "Italics" $ parseMaybe org "/Italic/"
      @?= Just [Paragraph [Italic "Italic"]]
    , testCase "Highlight" $ parseMaybe org "~Highlight~"
      @?= Just [Paragraph [Highlight "Highlight"]]
    , testCase "Verbatim" $ parseMaybe org "=Verbatim="
      @?= Just [Paragraph [Verbatim "Verbatim"]]
    , testCase "Underline" $ parseMaybe org "_Underline_"
      @?= Just [Paragraph [Underline "Underline"]]
    , testCase "Strike" $ parseMaybe org "+Strike+"
      @?= Just [Paragraph [Strike "Strike"]]
    , testCase "Link" $ parseMaybe org "[[https://www.fosskers.ca][Site]]"
      @?= Just [Paragraph [Link (URL "https://www.fosskers.ca") (Just "Site")]]
    , testCase "Link (no desc)" $ parseMaybe org "[[https://www.fosskers.ca]]"
      @?= Just [Paragraph [Link (URL "https://www.fosskers.ca") Nothing]]
    , testCase "Image" $ parseMaybe org "[[/path/to/img.jpeg]]"
      @?= Just [Paragraph [Image (URL "/path/to/img.jpeg")]]
    , testCase "Plain" $ parseMaybe org "This is a line"
      @?= Just [Paragraph [Plain "This", Plain "is", Plain "a", Plain "line"]]
    ]
  , testGroup "Markup Edge Cases"
    [ testCase "Before" $ parseMaybe org "This is *not*bold."
      @?= Just [Paragraph [Plain "This", Plain "is", Plain "*not*bold."]]
    , testCase "After" $ parseMaybe org "Neither is *this*here."
      @?= Just [Paragraph [Plain "Neither", Plain "is", Plain "*this*here."]]
    , testCase "Punctuation" $ parseMaybe org "*This*, is bold."
      @?= Just [Paragraph [Bold "This", Punct ',', Plain "is", Plain "bold."]]
    ]
  , testGroup "Composite Structures"
    [ testCase "Example" $ parseMaybe org "#+begin_example\nHi!\n\nHo\n#+end_example"
      @?= Just [Example "Hi!\n\nHo"]
    , testCase "Example - Empty" $ parseMaybe org "#+begin_example\n#+end_example"
      @?= Just [Example ""]
    , testCase "Quote" $ parseMaybe org "#+begin_quote\nHi!\n\nHo\n#+end_quote"
      @?= Just [Quote "Hi!\n\nHo"]
    , testCase "Quote - Empty" $ parseMaybe org "#+begin_quote\n#+end_quote"
      @?= Just [Quote ""]
    , testCase "Code" $ parseMaybe org "#+begin_src haskell\n1 + 1\n#+end_src"
      @?= Just [Code (Just $ Language "haskell") "1 + 1"]
    , testCase "Code - Empty" $ parseMaybe org "#+begin_src haskell\n#+end_src"
      @?= Just [Code (Just $ Language "haskell") ""]
    , testCase "Code - No Language" $ parseMaybe org "#+begin_src\n1 + 1\n#+end_src"
      @?= Just [Code Nothing "1 + 1"]
    , testCase "List" $ parseMaybe org "- A\n  - B\n- C"
      @?= Just [ List [Item 0 [Plain "A"], Item 1 [Plain "B"], Item 0 [Plain "C"]]]
    , testCase "List - Things after" $ do
        let! orig = parseMaybe org "- A\n  - B\n- C\n\nD"
        -- print $ prettyOrgs <$> orig
        orig @?= Just [ List [Item 0 [Plain "A"], Item 1 [Plain "B"], Item 0 [Plain "C"]]
                      , Paragraph [Plain "D"]]
    ]
  , testGroup "Pretty Printing"
    [ testCase "Punctuation" $ do
        let !orig = parseMaybe org "A /B/. C?"
        (prettyOrgs <$> orig) @?= Just "A /B/. C?"
    ]
  , testGroup "Full Files"
    [ testCase "Simple" $ do
        let !orig = parseMaybe org simple
        assertBool "Couldn't parse" $ isJust orig
        (orig >>= parseMaybe org . prettyOrgs) @?= orig
    , testCase "Full" $ case parse org "test.org" full of
        Left eb -> assertFailure $ errorBundlePretty eb
        Right r -> case parse org "test.org - reparse" (prettyOrgs r) of
          Left eb' -> assertFailure $ errorBundlePretty eb'
          Right r' -> r' @?= r
    ]
  ]
