module Data.Org
  ( -- * Types
    Org(..)
  , Words(..)
    -- * Parser
  , org
  ) where

import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- Types

-- | An org-mode document tree.
data Org = Heading Org
         | Quote Text
         | Example Text
         | Code Text
         | Paragraph [Words]
         | Meta

data Words = Bold Text
           | Italic Text
           | Highlight Text
           | Strikethrough Text
           | Plain Text

--------------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void Text

-- TODO Handle whitespace.
org :: Parser [Org]
org = many block

block :: Parser Org
block = choice [ heading, quote, example, code, paragraph, meta ]

heading :: Parser Org
heading = undefined

quote :: Parser Org
quote = undefined

example :: Parser Org
example = undefined

code :: Parser Org
code = undefined

paragraph :: Parser Org
paragraph = undefined

meta :: Parser Org
meta = undefined
