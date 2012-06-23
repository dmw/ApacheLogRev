---------------------------------------------------------------------------
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
  logLine
  , parseLogLine
  ) where



import Control.Applicative ((<|>))
import Control.DeepSeq (deepseq)

import Data.Attoparsec.ByteString.Char8 hiding (space, take)
import Data.Attoparsec.Combinator

import Data.LogRev.LogStats
import Text.Printf

import qualified Data.ByteString.Char8 as S


emptyString :: String
emptyString = ""


quote, lbrack, rbrack, eol, eolm, dash, dot :: Parser Char
space = satisfy (== ' ')
quote  = satisfy (== '\"')
lbrack = satisfy (== '[')
rbrack = satisfy (== ']')
eol = satisfy (== '\n')
eolm = satisfy (== '\r')
dash = satisfy (== '-')
dot = satisfy (== '.')


vhostClass :: String
vhostClass = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ".-_:"


numClass :: String
numClass = ['0' .. '9']


eolClass :: String
eolClass = "\r\n"


spaceClass :: String
spaceClass = "\r\n\t "


numbers :: Parser S.ByteString
numbers = takeTill (notInClass numClass)


vhosts :: Parser S.ByteString
vhosts = takeTill (notInClass vhostClass)


plainVal :: Parser S.ByteString
plainVal = takeTill (inClass spaceClass)


ipAddr :: Parser S.ByteString
ipAddr = takeTill (notInClass vhostClass)


bracketVal :: Parser S.ByteString
bracketVal = do
  lbrack
  content <- takeTill (== ']')
  rbrack
  return content


quotVal :: Parser S.ByteString
quotVal = do
  quote
  content <- takeTill (== '\"')
  quote
  return content


dashVal :: Parser S.ByteString
dashVal = takeTill (/= '-')


logLineVHost :: Parser LogLine
logLineVHost = do
  vhost <- vhosts
  space
  ip <- ipAddr
  space
  ident <- plainVal
  space
  user <- plainVal
  space
  date <- bracketVal
  space
  quotVal
  space
  status <- plainVal
  space
  bytes <- plainVal
  anyChar
  ref <- quotVal <|> dashVal
  space
  ua <- quotVal <|> dashVal
  return $ let
    _ret = LogLine {
      getVhost    = vhost
      , getIP     = ip
      , getIdent  = ident
      , getUser   = user
      , getDate   = date
      , getReq    = S.pack ""
      , getStatus = status
      , getBytes  = bytes
      , getRef    = ref
      , getUA     = ua
      }
    in _ret


logLineBasic :: Parser LogLine
logLineBasic = do
  ip <- ipAddr
  space
  ident <- plainVal
  space
  user <- plainVal
  space
  date <- bracketVal
  space
  quotVal
  space
  status <- plainVal
  space
  bytes <- plainVal
  space
  ref <- quotVal <|> dashVal
  space
  ua <- quotVal <|> dashVal
  return $ let
    _ret = LogLine {
      getVhost    = S.pack ""
      , getIP     = ip
      , getIdent  = ident
      , getUser   = user
      , getDate   = date
      , getReq    = S.pack ""
      , getStatus = status
      , getBytes  = bytes
      , getRef    = ref
      , getUA     = ua
      }
    in _ret


logLine :: Parser LogLine
logLine = logLineBasic
          <|> logLineVHost


parseLogLine :: String -> Either LogError LogLine
parseLogLine s = let
  l = S.pack s
  r = parse logLine l
  n = case r of
           Fail    p l r -> Left LogError { err = r , group = l }
           Partial     r -> Left LogError { err = "", group = [] }
           Done    p   r -> Right r
  in n `deepseq` n
