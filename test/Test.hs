{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Data.Org
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec

---

main :: IO ()
main = do
  simple <- T.readFile "test/simple.org"
  defaultMain $ suite simple

suite :: T.Text -> TestTree
suite simple = testGroup "Unit Tests"
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
    ]
  , testGroup "Full Files"
    [ testCase "Simple" $ do
        let !orig = parseMaybe org simple
        (orig >>= parseMaybe org . prettyOrgs) @?= orig
    ]
  ]
