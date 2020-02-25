{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Data.Org
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar (fromGregorian)
import           Data.Void (Void)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec

---

main :: IO ()
main = do
  simple <- T.readFile "test/simple.org"
  full   <- T.readFile "test/test.org"
  -- let fl = parseMaybe orgFile full
  -- pPrintNoColor fl
  -- maybe (putStrLn "COULDN'T PARSE") T.putStrLn $ prettyOrgFile <$> fl
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
    , testCase "Punctuation - Comma" $ parseMaybe org "*This*, is bold."
      @?= Just [Paragraph [Bold "This", Punct ',', Plain "is", Plain "bold."]]

    , testCase "Punctuation - Paren" $ parseMaybe org "(the ~be~)"
      @?= Just [Paragraph [Punct '(', Plain "the", Highlight "be", Punct ')']]

    , testCase "Punctuation - Double parens"
    $ testPretty paragraph "Parens" "(~Hello~)"
    $ Paragraph [Punct '(', Highlight "Hello", Punct ')']

    , testCase "Line - Plain" $ testPretty (line '\n') "Line" "A" [Plain "A"]
    , testCase "Line - Wide Gap" $ testPretty (line '\n') "Line" "A   B"
      [Plain "A", Plain "B"]
    , testCase "Line - Newline" $ testPretty (line '\n') "Line" "A\n" [Plain "A"]
    , testCase "Line - Space at end" $ testPretty (line '\n') "Line" "A \n" [Plain "A"]
    , testCase "Line - Dummy markup symbol" $ testPretty (line '\n') "Line" "A ~ B"
      [Plain "A", Plain "~", Plain "B"]
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
      @?= Just [List [Item 0 [Plain "A"], Item 1 [Plain "B"], Item 0 [Plain "C"]]]
    , testCase "List - Things after" $ parseMaybe org "- A\n  - B\n- C\n\nD"
      @?= Just [List [Item 0 [Plain "A"], Item 1 [Plain "B"], Item 0 [Plain "C"]]
               , Paragraph [Plain "D"]]

    , testCase "List - Multiline"
      $ testPretty list "List" "- A\n  B\n- C"
      $ List [Item 0 [Plain "A", Plain "B"], Item 0 [Plain "C"]]

    , testCase "Table - One Row"
      $ testPretty table "Table" "| A | B | C |"
      $ Table [Row [Column [Plain "A"], Column [Plain "B"], Column [Plain "C"]]]

    , testCase "Table - One Row and Break"
      $ testPretty table "Table" "| A | B | C |\n|---+---+---|"
      $ Table [ Row [Column [Plain "A"], Column [Plain "B"], Column [Plain "C"]]
              , Break ]
    , testCase "Table - Row, Break, Row"
      $ testPretty table "Table" "| A | B | C |\n|---+---+---|\n| D | E | F |"
      $ Table [ Row [Column [Plain "A"], Column [Plain "B"], Column [Plain "C"]]
              , Break
              , Row [Column [Plain "D"], Column [Plain "E"], Column [Plain "F"]]]
    , testCase "Table - Markup"
      $ testPretty table "Table" "| *A* Yes | /B/ No |"
      $ Table [Row [Column [Bold "A", Plain "Yes"], Column [Italic "B", Plain "No"]]]
    , testCase "Table - Empty Column"
      $ testPretty table "Table" "| A | | C |"
      $ Table [Row [Column [Plain "A"], Empty, Column [Plain "C"]]]
    , testCase "Meta - Title"
      $ testPretty meta "Meta" "#+TITLE: Test"
      $ Meta (Just "Test") Nothing Nothing Nothing
    , testCase "Meta - Full"
      $ testPretty meta "Meta" "#+TITLE: Test\n#+DATE: 2020-02-17\n#+AUTHOR: Colin"
      $ Meta (Just "Test") (Just $ fromGregorian 2020 2 17) (Just "Colin") Nothing
    ]
  , testGroup "Pretty Printing"
    [ testCase "Punctuation" $ do
        let !orig = parseMaybe org "A /B/. C?"
        (prettyOrgs <$> orig) @?= Just "A /B/. C?"
    ]
  , testGroup "Full Files"
    [ testCase "Simple" $ case parse orgFile "simple.org" simple of
        Left eb -> assertFailure $ errorBundlePretty eb
        Right r -> case parse orgFile "simple.org - reparse" (prettyOrgFile r) of
          Left eb' -> assertFailure $ errorBundlePretty eb'
          Right r' -> r' @?= r
    , testCase "Full" $ case parse orgFile "test.org" full of
        Left eb -> assertFailure $ errorBundlePretty eb
        Right r -> case parse orgFile "test.org - reparse" (prettyOrgFile r) of
          Left eb' -> assertFailure $ errorBundlePretty eb'
          Right r' -> r' @?= r
    ]
  ]

testPretty :: (Eq a, Show a) => Parsec Void Text a -> String -> Text -> a -> Assertion
testPretty parser labl src expt = case parse parser labl src of
  Left eb -> assertFailure $ errorBundlePretty eb
  Right r -> r @?= expt
