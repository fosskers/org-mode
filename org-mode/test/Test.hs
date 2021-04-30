{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Data.Org
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar (DayOfWeek(..), fromGregorian)
import           Data.Time.LocalTime (TimeOfDay(..))
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
  -- let fl = parseMaybe orgFile full
  -- pPrintNoColor fl
  -- maybe (putStrLn "COULDN'T PARSE") T.putStrLn $ prettyOrgFile <$> fl
  defaultMain $ suite simple full

suite :: T.Text -> T.Text -> TestTree
suite simple full = testGroup "Unit Tests"
  [ testGroup "Basic Markup"
    [ testCase "Header" $ parseMaybe (section 1) "* A" @?= Just (titled (Plain "A"))
    , testCase "Header - Subsection" $ parseMaybe (section 1) "* A\n** B"
      @?= Just ((titled (Plain "A")) { sectionDoc = OrgDoc [] [titled (Plain "B")] })

    , testCase "Header - Back again"
      $ testPretty orgP "Header" "* A\n** B\n* C"
      $ OrgDoc [] [ (titled (Plain "A")) { sectionDoc = OrgDoc [] [titled (Plain "B")] }, titled (Plain "C") ]

    , testCase "Header - Contents"
      $ testPretty orgP "Header" "* A\nD\n\n** B\n* C"  -- TODO Requires an extra newline!
      $ OrgDoc []
      [ (titled (Plain "A")) { sectionDoc = OrgDoc [Paragraph [Plain "D"]] [ titled (Plain "B") ] }
      , titled (Plain "C") ]

    , testCase "Header - TODO"
      $ testPretty orgP "Header" "* TODO A"
      $ OrgDoc []
      [ (titled (Plain "A")) { sectionTodo = Just TODO }]

    , testCase "Header - Priority"
      $ testPretty orgP "Header" "* [#A] A"
      $ OrgDoc []
      [ (titled (Plain "A")) { sectionPriority = Just $ Priority "A" }]

    , testCase "Header - TODO + Priority"
      $ testPretty orgP "Header" "* TODO [#A] A"
      $ OrgDoc []
      [ (titled (Plain "A")) { sectionTodo = Just TODO, sectionPriority = Just $ Priority "A" }]

    , testCase "Header - One line, single tag"
      $ testPretty orgP "Header" "* A  :this:"
      $ OrgDoc [] [ (titled (Plain "A")) { sectionTags = ["this"] } ]

    , testCase "Header - One line, multiple tags"
      $ testPretty orgP "Header" "* A  :this:that:"
      $ OrgDoc [] [ (titled (Plain "A")) { sectionTags = ["this", "that"] } ]

    , testCase "Header - More Tags"
      $ testPretty orgP "Header" "* A  :this:that:\n** B   :other:\n* C"
      $ OrgDoc []
      [ (titled (Plain "A"))
        { sectionTags = ["this", "that"]
        , sectionDoc = OrgDoc [] [ (titled (Plain "B")) { sectionTags = ["other"] } ] }
      , titled (Plain "C") ]

    , testCase "Header - Non-tag Smiley"
      $ testPretty orgP "Header" "* A :)"
      $ OrgDoc [] [ Section Nothing Nothing [Plain "A", Punct ':', Punct ')'] [] Nothing Nothing Nothing Nothing mempty emptyDoc ]

    , testCase "Header - Vanilla Timestamp"
      $ testPretty orgP "Header" "* A\n  <2021-04-19 Mon>"
      $ let tm = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
        in OrgDoc [] [ (titled (Plain "A")) { sectionTimestamp = Just tm } ]

    , testCase "Header - CLOSED"
      $ testPretty orgP "Header" "* A\n  CLOSED: [2021-04-19 Mon 15:43]"
      $ let cl = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Just $ OrgTime (TimeOfDay 15 43 0) Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
        in OrgDoc [] [ (titled (Plain "A")) { sectionClosed = Just cl } ]

    , testCase "Header - DEADLINE"
      $ testPretty orgP "Header" "* A\n  DEADLINE: <2021-04-19 Mon>"
      $ let dl = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
        in OrgDoc [] [ (titled (Plain "A")) { sectionDeadline = Just dl } ]

    , testCase "Header - SCHEDULED"
      $ testPretty orgP "Header" "* A\n  SCHEDULED: <2021-04-19 Mon>"
      $ let sc = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
        in OrgDoc [] [ (titled (Plain "A")) { sectionScheduled = Just sc } ]

    , testCase "Header - CLOSED/DEADLINE"
      $ testPretty orgP "Header" "* A\n  CLOSED: [2021-04-19 Mon 15:43] DEADLINE: <2021-04-19 Mon>"
      $ let dl = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
            cl = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Just $ OrgTime (TimeOfDay 15 43 0) Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
        in OrgDoc [] [ (titled (Plain "A")) { sectionClosed = Just cl, sectionDeadline = Just dl } ]

    , testCase "Header - CLOSED/DEADLINE - More"
      $ testPretty orgP "Header" "* A\n  CLOSED: [2021-04-19 Mon 15:43] DEADLINE: <2021-04-19 Mon>\nD"
      $ let dl = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
            cl = OrgDateTime { dateDay = fromGregorian 2021 4 19
                             , dateDayOfWeek = Monday
                             , dateTime = Just $ OrgTime (TimeOfDay 15 43 0) Nothing
                             , dateRepeat = Nothing
                             , dateDelay = Nothing }
        in OrgDoc [] [ Section Nothing Nothing [Plain "A"] [] (Just cl) (Just dl) Nothing Nothing mempty (OrgDoc [ Paragraph [Plain "D"]] []) ]

    , testCase "Header - Empty Properties Drawer"
      $ testPretty orgP "Header" "* A\n  :PROPERTIES:\n  :END:"
      $ OrgDoc [] [ titled (Plain "A") ]

    , testCase "Header - One Property"
      $ testPretty orgP "Header" "* A\n  :PROPERTIES:\n  :Cat: Jack\n  :END:\nHi"
      $ OrgDoc [] [ (titled (Plain "A"))
                    { sectionProps = [("Cat", "Jack")]
                    , sectionDoc = OrgDoc [Paragraph [Plain "Hi"]] [] }]

    , testCase "Header - Two Properties"
      $ testPretty orgP "Header" "* A\n  :PROPERTIES:\n  :Cat: Jack\n  :Age: 7\n  :END:"
      $ OrgDoc [] [ (titled (Plain "A")) { sectionProps = [("Cat", "Jack"), ("Age", "7")] } ]

    , testCase "Properties"
      $ testPretty properties "Properties" ":PROPERTIES:\n  :Cat: Jack\n  :END:" [("Cat", "Jack")]
    , testCase "Property" $ testPretty property "Property" ":Cat: Jack" ("Cat", "Jack")

    , testCase "Bold" $ parseMaybe orgP "*Bold*"
      @?= Just (OrgDoc [Paragraph [Bold "Bold"]] [])
    , testCase "Italics" $ parseMaybe orgP "/Italic/"
      @?= Just (OrgDoc [Paragraph [Italic "Italic"]] [])
    , testCase "Highlight" $ parseMaybe orgP "~Highlight~"
      @?= Just (OrgDoc [Paragraph [Highlight "Highlight"]] [])
    , testCase "Verbatim" $ parseMaybe orgP "=Verbatim="
      @?= Just (OrgDoc [Paragraph [Verbatim "Verbatim"]] [])
    , testCase "Underline" $ parseMaybe orgP "_Underline_"
      @?= Just (OrgDoc [Paragraph [Underline "Underline"]] [])
    , testCase "Strike" $ parseMaybe orgP "+Strike+"
      @?= Just (OrgDoc [Paragraph [Strike "Strike"]] [])

    , testCase "Link" $ parseMaybe orgP "[[https://www.fosskers.ca][Site]]"
      @?= Just (OrgDoc [Paragraph [Link (URL "https://www.fosskers.ca") (Just "Site")]] [])
    , testCase "Link (no desc)" $ parseMaybe orgP "[[https://www.fosskers.ca]]"
      @?= Just (OrgDoc [Paragraph [Link (URL "https://www.fosskers.ca") Nothing]] [])

    , testCase "Image" $ parseMaybe orgP "[[/path/to/img.jpeg]]"
      @?= Just (OrgDoc [Paragraph [Image (URL "/path/to/img.jpeg")]] [])
    , testCase "Image - False Positive"
      $ testPretty paragraph "Image" "[[http://a.ca][hi]] [[a.png]]"
      $ Paragraph [Link (URL "http://a.ca") (Just "hi"), Image (URL "a.png")]

    , testCase "Plain" $ parseMaybe orgP "This is a line"
      @?= Just (OrgDoc [Paragraph [Plain "This", Plain "is", Plain "a", Plain "line"]] [])
    ]
  , testGroup "Markup Edge Cases"
    [ testCase "Before" $ parseMaybe orgP "This is *not*bold."
      @?= Just (OrgDoc [Paragraph [Plain "This", Plain "is", Plain "*not*bold."]] [])
    , testCase "After" $ parseMaybe orgP "Neither is *this*here."
      @?= Just (OrgDoc [Paragraph [Plain "Neither", Plain "is", Plain "*this*here."]] [])
    , testCase "Punctuation - Comma" $ parseMaybe orgP "*This*, is bold."
      @?= Just (OrgDoc [Paragraph [Bold "This", Punct ',', Plain "is", Plain "bold."]] [])

    , testCase "Punctuation - Paren" $ parseMaybe orgP "(the ~be~)"
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
  , testGroup "Timestamps"
    [ testCase "Repeater" $ testPretty repeater "Repeater" "+2w" (Repeater Single 2 Week)
    , testCase "Time - Morning" $ testPretty timeRange "Time" "09:30" (OrgTime (TimeOfDay 9 30 0) Nothing)
    , testCase "Time - Afternoon" $ testPretty timeRange "Time" "14:30" (OrgTime (TimeOfDay 14 30 0) Nothing)
    , testCase "Time - Range" $ testPretty timeRange "Time" "09:30-14:30" (OrgTime (TimeOfDay 9 30 0) (Just (TimeOfDay 14 30 0)))
    , testCase "Date" $ testPretty date "Date" "2021-04-27" $ fromGregorian 2021 4 27
    , testCase "Timestamp - No time" $ testPretty timestamp "Stamp" "2021-04-27 Tue"
      $ OrgDateTime
      { dateDay = fromGregorian 2021 4 27
      , dateDayOfWeek = Tuesday
      , dateTime = Nothing
      , dateRepeat = Nothing
      , dateDelay = Nothing }
    , testCase "Timestamp - Time" $ testPretty timestamp "Stamp" "2021-04-27 Tue 09:30"
      $ OrgDateTime
      { dateDay = fromGregorian 2021 4 27
      , dateDayOfWeek = Tuesday
      , dateTime = Just $ OrgTime (TimeOfDay 9 30 0) Nothing
      , dateRepeat = Nothing
      , dateDelay = Nothing }
    , testCase "Timestamp - Repeat Single" $ testPretty timestamp "Stamp" "2021-04-27 Tue +2w"
      $ OrgDateTime
      { dateDay = fromGregorian 2021 4 27
      , dateDayOfWeek = Tuesday
      , dateTime = Nothing
      , dateRepeat = Just $ Repeater Single 2 Week
      , dateDelay = Nothing }
    , testCase "Timestamp - Repeat Jump" $ testPretty timestamp "Stamp" "2021-04-27 Tue ++2w"
      $ OrgDateTime
      { dateDay = fromGregorian 2021 4 27
      , dateDayOfWeek = Tuesday
      , dateTime = Nothing
      , dateRepeat = Just $ Repeater Jump 2 Week
      , dateDelay = Nothing }
    , testCase "Timestamp - Time and Repeat" $ testPretty timestamp "Stamp" "2021-04-27 Tue 09:30 +2w"
      $ OrgDateTime
      { dateDay = fromGregorian 2021 4 27
      , dateDayOfWeek = Tuesday
      , dateTime = Just $ OrgTime (TimeOfDay 9 30 0) Nothing
      , dateRepeat = Just $ Repeater Single 2 Week
      , dateDelay = Nothing }
    , testCase "Timestamp - Delay" $ testPretty timestamp "Stamp" "2021-04-27 Tue -3d"
      $ OrgDateTime
      { dateDay = fromGregorian 2021 4 27
      , dateDayOfWeek = Tuesday
      , dateTime = Nothing
      , dateRepeat = Nothing
      , dateDelay = Just $ Delay DelayAll 3 Day }
    , testCase "Timestamp - Repeat and Delay" $ testPretty timestamp "Stamp" "2021-04-27 Tue +2w -3d"
      $ OrgDateTime
      { dateDay = fromGregorian 2021 4 27
      , dateDayOfWeek = Tuesday
      , dateTime = Nothing
      , dateRepeat = Just $ Repeater Single 2 Week
      , dateDelay = Just $ Delay DelayAll 3 Day }
    ]
  , testGroup "Composite Structures"
    [ testCase "Example" $ parseMaybe orgP "#+begin_example\nHi!\n\nHo\n#+end_example"
      @?= Just (OrgDoc [Example "Hi!\n\nHo"] [])
    , testCase "Example - Empty" $ parseMaybe orgP "#+begin_example\n#+end_example"
      @?= Just (OrgDoc [Example ""] [])
    , testCase "Quote" $ parseMaybe orgP "#+begin_quote\nHi!\n\nHo\n#+end_quote"
      @?= Just (OrgDoc [Quote "Hi!\n\nHo"] [])
    , testCase "Quote - Empty" $ parseMaybe orgP "#+begin_quote\n#+end_quote"
      @?= Just (OrgDoc [Quote ""] [])
    , testCase "Code" $ parseMaybe orgP "#+begin_src haskell\n1 + 1\n#+end_src"
      @?= Just (OrgDoc [Code (Just $ Language "haskell") "1 + 1"] [])
    , testCase "Code - Empty" $ parseMaybe orgP "#+begin_src haskell\n#+end_src"
      @?= Just (OrgDoc [Code (Just $ Language "haskell") ""] [])
    , testCase "Code - No Language" $ parseMaybe orgP "#+begin_src\n1 + 1\n#+end_src"
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

    , testCase "List - Things after" $ parseMaybe orgP "- A\n  - B\n- C\n\nD"
      @?= Just (OrgDoc [ List (ListItems
                               [ Item [Plain "A"] (Just $ ListItems [Item [Plain "B"] Nothing])
                               , Item [Plain "C"] Nothing])
                       , Paragraph [Plain "D"]] [])

    , testCase "List - Multiline"
      $ testPretty list "List" "- A\n  B\n- C"
      $ List (ListItems [Item [Plain "A", Plain "B"] Nothing, Item [Plain "C"] Nothing])

    , testCase "List - Special Parens"
      $ testPretty list "List" "- The [[https://github.com/kadena-io/chainweb-node][A]] (core developer)\n- B"
      $ List (ListItems
              [ Item [Plain "The", Link (URL "https://github.com/kadena-io/chainweb-node") (Just "A"), Punct '(', Plain "core", Plain "developer)"] Nothing
              , Item [Plain "B"] Nothing ]
             )

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
      [("TITLE", "Test")]
    , testCase "Meta - Full"
      $ testPretty meta "Meta" "#+TITLE: Test\n#+DATE: 2020-02-17\n#+AUTHOR: Colin"
      [("TITLE", "Test"), ("DATE", "2020-02-17"), ("AUTHOR", "Colin")]
    ]
  , testGroup "Pretty Printing"
    [ testCase "Punctuation" $ do
        let !orig = parseMaybe orgP "A /B/. C?"
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
    , testCase "Full: Tag Extraction"
      $ (allDocTags . orgDoc <$> parse orgFile "test.org" full) @?= Right ["tag1", "tag2", "tag3"]
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
