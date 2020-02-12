{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Org
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
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
  , testGroup "Complex Files" []
  ]
