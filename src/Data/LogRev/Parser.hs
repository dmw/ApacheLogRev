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


import Control.DeepSeq

import Data.LogRev.LogStats
import Text.ParserCombinators.Parsec
import Text.Printf


emptyString :: String
emptyString = ""


startLogLine :: LogLine
startLogLine = LogLine {
  getVhost    = ""
  , getIP     = ""
  , getIdent  = ""
  , getUser   = ""
  , getDate   = ""
  , getReq    = ""
  , getStatus = ""
  , getBytes  = ""
  , getRef    = ""
  , getUA     = ""
  }


validNumberChars :: String
validNumberChars = ['0' .. '9']

validVHostChars :: String
validVHostChars = ['0' .. '9']
                   ++ ['a' .. 'z']
                   ++ ['A' .. 'Z']
                   ++ ".-_:"

plainValue :: Parser String
plainValue = do x <- many1 (noneOf " \n")
                return (x `seq` x)

parseVHost :: GenParser Char st String
parseVHost = do x <- many $ oneOf validVHostChars
                return (x `seq` x)

parseIP :: GenParser Char st String
parseIP = do o1 <- many $ oneOf validNumberChars
             _  <- char '.'
             o2 <- many $ oneOf validNumberChars
             _  <- char '.'
             o3 <- many $ oneOf validNumberChars
             _  <- char '.'
             o4 <- many $ oneOf validNumberChars
             return $ let x = (printf "%s.%s.%s.%s" o1 o2 o3 o4)
                          in x `seq` x

bracketedValue :: Parser String
bracketedValue = do
  _ <- char '['
  content <- many (noneOf "]")
  _ <- char ']'
  return (content `seq` content)

quotedValue :: Parser String
quotedValue = do
  _ <- char '"'
  content <- many (noneOf "\"")
  _ <- char '"'
  return (content `seq` content)

dashChar :: Parser String
dashChar = do x <- char '-'
              return (show x)

logLineVHost :: Parser LogLine
logLineVHost = do
  vhost <- parseVHost
  _ <- space
  ip <- parseIP
  _ <- space
  ident <- plainValue
  _ <- space
  user <- plainValue
  _ <- space
  date <- bracketedValue
  _ <- space
  _ <- quotedValue
  _ <- space
  status <- plainValue
  _ <- space
  bytes <- plainValue
  _ <- space
  ref <- dashChar <|> quotedValue
  _ <- space
  ua <- dashChar <|> quotedValue
  return startLogLine {
    getVhost    = vhost
    , getIP     = ip
    , getIdent  = ident
    , getUser   = user
    , getDate   = date
    , getReq    = emptyString
    , getStatus = status
    , getBytes  = bytes
    , getRef    = ref
    , getUA     = ua
    }

logLineBasic :: Parser LogLine
logLineBasic = do
  ip <- parseIP
  _ <- space
  ident <- plainValue
  _ <- space
  user <- plainValue
  _ <- space
  date <- bracketedValue
  _ <- space
  _ <- quotedValue
  _ <- space
  status <- plainValue
  _ <- space
  bytes <- plainValue
  _ <- space
  ref <- dashChar <|> quotedValue
  _ <- space
  ua <- dashChar <|> quotedValue
  return startLogLine {
    getVhost    = emptyString
    , getIP     = ip
    , getIdent  = ident
    , getUser   = user
    , getDate   = date
    , getReq    = emptyString
    , getStatus = status
    , getBytes  = bytes
    , getRef    = ref
    , getUA     = ua
    }

logLine :: Parser LogLine
logLine = logLineBasic <|> logLineVHost


parseLogLine :: String -> Maybe LogLine
parseLogLine ~s = let
  r = parse logLine "[Invalid]" s
  in case r of
          Left  _   -> Nothing
          Right itm -> Just itm

