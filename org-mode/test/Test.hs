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
import           Text.Megaparsec.Char

---

main :: IO ()
main = do
  simple <- T.readFile "test/simple.org"
  full   <- T.readFile "test/test.org"
  let fl = parseMaybe orgFile full
  -- pPrintNoColor fl
  maybe (putStrLn "COULDN'T PARSE") T.putStrLn $ prettyOrgFile <$> fl
  defaultMain $ suite simple full

suite :: T.Text -> T.Text -> TestTree
suite simple full = testGroup "Unit Tests"
  [ testGroup "Basic Markup"
    [ testCase "Header" $ parseMaybe (section 1) "* A"
      @?= Just (Section [Plain "A"] emptyDoc)
    , testCase "Headers" $ parseMaybe (section 1) "* A\n** B"
      @?= Just (Section [Plain "A"] (OrgDoc [] [Section [Plain "B"] emptyDoc]))

    , testCase "Bold" $ parseMaybe org "*Bold*"
      @?= Just (OrgDoc [Paragraph [Bold "Bold"]] [])
    , testCase "Italics" $ parseMaybe org "/Italic/"
      @?= Just (OrgDoc [Paragraph [Italic "Italic"]] [])
    , testCase "Highlight" $ parseMaybe org "~Highlight~"
      @?= Just (OrgDoc [Paragraph [Highlight "Highlight"]] [])
    , testCase "Verbatim" $ parseMaybe org "=Verbatim="
      @?= Just (OrgDoc [Paragraph [Verbatim "Verbatim"]] [])
    , testCase "Underline" $ parseMaybe org "_Underline_"
      @?= Just (OrgDoc [Paragraph [Underline "Underline"]] [])
    , testCase "Strike" $ parseMaybe org "+Strike+"
      @?= Just (OrgDoc [Paragraph [Strike "Strike"]] [])

    , testCase "Link" $ parseMaybe org "[[https://www.fosskers.ca][Site]]"
      @?= Just (OrgDoc [Paragraph [Link (URL "https://www.fosskers.ca") (Just "Site")]] [])
    , testCase "Link (no desc)" $ parseMaybe org "[[https://www.fosskers.ca]]"
      @?= Just (OrgDoc [Paragraph [Link (URL "https://www.fosskers.ca") Nothing]] [])

    , testCase "Image" $ parseMaybe org "[[/path/to/img.jpeg]]"
      @?= Just (OrgDoc [Paragraph [Image (URL "/path/to/img.jpeg")]] [])
    , testCase "Image - False Positive"
      $ testPretty paragraph "Image" "[[http://a.ca][hi]] [[a.png]]"
      $ Paragraph [Link (URL "http://a.ca") (Just "hi"), Image (URL "a.png")]

    , testCase "Plain" $ parseMaybe org "This is a line"
      @?= Just (OrgDoc [Paragraph [Plain "This", Plain "is", Plain "a", Plain "line"]] [])
    ]
  , testGroup "Markup Edge Cases"
    [ testCase "Before" $ parseMaybe org "This is *not*bold."
      @?= Just (OrgDoc [Paragraph [Plain "This", Plain "is", Plain "*not*bold."]] [])
    , testCase "After" $ parseMaybe org "Neither is *this*here."
      @?= Just (OrgDoc [Paragraph [Plain "Neither", Plain "is", Plain "*this*here."]] [])
    , testCase "Punctuation - Comma" $ parseMaybe org "*This*, is bold."
      @?= Just (OrgDoc [Paragraph [Bold "This", Punct ',', Plain "is", Plain "bold."]] [])

    , testCase "Punctuation - Paren" $ parseMaybe org "(the ~be~)"
      @?= Just (OrgDoc [Paragraph [Punct '(', Plain "the", Highlight "be", Punct ')']] [])

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
      @?= Just (OrgDoc [Example "Hi!\n\nHo"] [])
    , testCase "Example - Empty" $ parseMaybe org "#+begin_example\n#+end_example"
      @?= Just (OrgDoc [Example ""] [])
    , testCase "Quote" $ parseMaybe org "#+begin_quote\nHi!\n\nHo\n#+end_quote"
      @?= Just (OrgDoc [Quote "Hi!\n\nHo"] [])
    , testCase "Quote - Empty" $ parseMaybe org "#+begin_quote\n#+end_quote"
      @?= Just (OrgDoc [Quote ""] [])
    , testCase "Code" $ parseMaybe org "#+begin_src haskell\n1 + 1\n#+end_src"
      @?= Just (OrgDoc [Code (Just $ Language "haskell") "1 + 1"] [])
    , testCase "Code - Empty" $ parseMaybe org "#+begin_src haskell\n#+end_src"
      @?= Just (OrgDoc [Code (Just $ Language "haskell") ""] [])
    , testCase "Code - No Language" $ parseMaybe org "#+begin_src\n1 + 1\n#+end_src"
      @?= Just (OrgDoc [Code Nothing "1 + 1"] [])

     , testCase "List - Single Indent"
      $ testPretty list "List" "- A\n  - B"
      $ List (ListItems [Item [Plain "A"] (Just $ ListItems [Item [Plain "B"] Nothing])])

    , testCase "List - Indent and Back"
      $ testPretty list "List" "- A\n  - B\n- C"
      $ List (ListItems
               [ Item [Plain "A"] (Just $ ListItems [Item [Plain "B"] Nothing])
               , Item [Plain "C"] Nothing ])

    , testCase "List - Double Indent and Back"
      $ testPretty list "List" "- A\n  - B\n  - C\n- D"
      $ List (ListItems
               [ Item [Plain "A"] (Just $ ListItems [Item [Plain "B"] Nothing, Item [Plain "C"] Nothing])
               , Item [Plain "D"] Nothing ])

    , testCase "List - Things after" $ parseMaybe org "- A\n  - B\n- C\n\nD"
      @?= Just (OrgDoc [ List (ListItems
                               [ Item [Plain "A"] (Just $ ListItems [Item [Plain "B"] Nothing])
                               , Item [Plain "C"] Nothing])
                       , Paragraph [Plain "D"]] [])

    , testCase "List - Multiline"
      $ testPretty list "List" "- A\n  B\n- C"
      $ List (ListItems [Item [Plain "A", Plain "B"] Nothing, Item [Plain "C"] Nothing])

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
      $ Meta (Just "Test") Nothing Nothing Nothing Nothing
    , testCase "Meta - Full"
      $ testPretty meta "Meta" "#+TITLE: Test\n#+DATE: 2020-02-17\n#+AUTHOR: Colin"
      $ Meta (Just "Test") (Just $ fromGregorian 2020 2 17) (Just "Colin") Nothing Nothing
    ]
  , testGroup "Pretty Printing"
    [ testCase "Punctuation" $ do
        let !orig = parseMaybe org "A /B/. C?"
        (prettyOrg <$> orig) @?= Just "A /B/. C?"
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
  , testGroup "Megaparsec Sanity"
    [ testCase "sepEndBy1" $ testPretty sepTest "sepBy1" "A.A.A.B" ['A', 'A', 'A']
    ]
  ]

testPretty :: (Eq a, Show a) => Parsec Void Text a -> String -> Text -> a -> Assertion
testPretty parser labl src expt = case parse parser labl src of
  Left eb -> assertFailure $ errorBundlePretty eb
  Right r -> r @?= expt

-- | Conclusion: If the separator appears but the next parse attempt fails, the
-- /whole/ thing fails.
sepTest :: Parsec Void Text String
sepTest = char 'A' `sepEndBy1` char '.'
