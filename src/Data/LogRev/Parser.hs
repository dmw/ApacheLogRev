----------------------------------------------------------------------------
-- |
-- Module      :  Data.LogRev.Parser
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/logrev/
-- Repository  :  https://github.com/dmw/ApacheLogRev
--
-- An Apache Access Log Statistics extractor modules
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------


module Data.LogRev.Parser (
  plainValue
  , bracketedValue
  , quotedValue
  , dashChar
  , logLine
  , parseLogLine
  ) where


import Data.LogRev.LogStats
import Text.ParserCombinators.Parsec


plainValue :: Parser String
plainValue = many1 (noneOf " \n")

bracketedValue :: Parser String
bracketedValue = do
  char '['
  content <- many (noneOf "]")
  char ']'
  return content

quotedValue :: Parser String
quotedValue = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return content

dashChar :: Parser String
dashChar = do
  x <- char '-'
  return (show x)

logLine :: Parser LogLine
logLine = do
  vhost <- plainValue
  space
  ip <- plainValue
  space
  ident <- plainValue
  space
  user <- plainValue
  space
  date <- bracketedValue
  space
  req <- quotedValue
  space
  status <- plainValue
  space
  bytes <- plainValue
  space
  ref <- dashChar <|> quotedValue
  space
  ua <- dashChar <|> quotedValue
  return $ LogLine vhost ip ident user date req status bytes ref ua

parseLogLine :: String -> Maybe LogLine
parseLogLine s = let !r = parse logLine "[Invalid]" s
                     in case r of
                             Left perr -> Nothing
                             Right itm -> Just itm
