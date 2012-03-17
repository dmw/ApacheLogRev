----------------------------------------------------------------------------
-- |
-- Module      :  Proc.LRS.Parser
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


module Proc.LRS.Parser (
  parseLRS
  , LRS (..)
  ) where


import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.Printf

data LRSType = LRSBuiltIn
             | LRSCustom
               deriving (Eq, Show)

data LRSChartType = LRSChartBar
                  | LRSChartPie
                  | LRSChartLine
                    deriving (Eq, Show)

data LRS = LRS {
  specId         :: String
  , specTitle    :: String
  , specRules    :: [LRSRule]
  , specTokens   :: [String]
  , specReport   :: [LRSReport]
  } deriving (Eq, Show)

data LRSParam = LRSParam {
  name           :: String
  , value        :: String
  } deriving (Eq, Show)

data LRSRule = LRSRule {
  ruleId         :: String
  , ruleSpec     :: LRSType
  , ruleProc     :: String
  , ruleParams   :: [LRSParam]
  } deriving (Eq, Show)

data LRSReport = LRSReport {
  reportId       :: String
  , reportTitle  :: String
  , reportVars   :: [LRSVar]
  , reportTables :: [LRSTable]
  , reportCharts :: [LRSChart]
  , reportSpec   :: [LRSSpecifier]
  } deriving (Eq, Show)

data LRSVar = LRSVar {
  varName        :: String
  , varFun       :: String
  , varApp       :: [String]
  , varFil       :: String
  } deriving (Eq, Show)

data LRSTable = LRSTable {
  tabId          :: String
  , tabGroup     :: String
  , tabVars      :: [String]
  , tabTitle     :: String
  } deriving (Eq, Show)

data LRSChart = LRSChart {
  chartId        :: String
  , chartGroup   :: String
  , chartVars    :: [String]
  , chartTitle   :: String
  , chartType    :: LRSChartType
  } deriving (Eq, Show)

data LRSSpecifier = LRSSpecifier {
  specName       :: String
  , specVal      :: String
  } deriving (Eq, Show)


readEol :: GenParser Char st String
readEol = many (char '\n' <|> char '\r')

identifierValue :: Parser String
identifierValue = many (noneOf "\r\n\t :#><[]=$|,")

textValue :: Parser String
textValue = many (noneOf "\r\n$#><[]\"")

singleQuoteValue :: Parser String
singleQuoteValue = many (noneOf "\'")

doubleQuoteValue :: Parser String
doubleQuoteValue = many (noneOf "\"")

parseSection :: String -> Parser String
parseSection s = do
  string s
  _ <- spaces
  res <- identifierValue
  _ <- readEol
  return res

parseSectionText :: String -> Parser String
parseSectionText s = do
  _ <- string s >> spaces >> char '"'
  res <- textValue
  _ <- char '"' >> readEol
  return res

logSpecHeader :: Parser LRS
logSpecHeader = do
  specId <- parseSection "log-spec:"
  _ <- spaces
  specTitle <- parseSectionText "log-title:"
  return LRS { specId      = specId
             , specTitle   = specTitle
             , specRules   = []
             , specTokens  = []
             , specReport  = []
             }

parseParam :: Parser LRSParam
parseParam = do
  _ <- string "param:" >> spaces >> char ':'
  vn <- identifierValue
  _ <- char ':' >> char '\''
  vl <- singleQuoteValue
  _ <- char '\'' >> readEol
  return LRSParam { name   = vn
                  , value  = vl
                  }

parseBuiltIn :: Parser LRSType
parseBuiltIn = do
  string ":built-in:"
  return LRSBuiltIn

parseCustom :: Parser LRSType
parseCustom = do
  string ":custom:"
  return LRSCustom

logRuleParserParam :: Parser LRSRule
logRuleParserParam = do
  _ <- spaces
  ruleId <- parseSection "log-rule:"
  _ <- spaces >> string "log-is:" >> spaces
  lt <- (parseBuiltIn <|> parseCustom)
  pr <- identifierValue
  _ <- spaces
  pm <- many parseParam
  _ <- spaces >> string ":end-rule:" >> readEol
  return LRSRule { ruleId     = ruleId
                 , ruleSpec   = lt
                 , ruleProc   = pr
                 , ruleParams = pm
                 }

logTokenParser :: Parser String
logTokenParser = do
  _ <- spaces >> char '|' >> spaces
  i <- identifierValue
  _ <- readEol
  return i

logTokenizerParser :: Parser [String]
logTokenizerParser = do
  _ <- string "log-tokenize:"
  tokens <- many logTokenParser
  _ <- string ":end-tokenize:" >> readEol
  return tokens

logTokenVars :: Parser String
logTokenVars = char '$' >> identifierValue

logParseVar :: Parser LRSVar
logParseVar = do
  _ <- spaces
  n <- parseSection "define-var:"
  _ <- spaces >> string "define-value:" >> spaces
  _ <- string ":function:"
  f <- identifierValue
  _ <- char '='
  a <- many logTokenVars
  _ <- char '|'
  l <- identifierValue
  _ <- readEol >> spaces >> string ":end-var:" >> readEol
  return LRSVar { varName = n
                , varFun  = f
                , varApp  = a
                , varFil  = l
                }

commaDelimVal :: Parser String
commaDelimVal = do
  x <- identifierValue
  _ <- (char '\n' <|> char '\r' <|> char ',')
  return x

groupColParser :: Parser (String, [String])
groupColParser = do
  g <- identifierValue
  _ <- char '|'
  v <- many commaDelimVal
  return (g, v)

logParseTable :: Parser LRSTable
logParseTable = do
  _ <- spaces >> string "define-table:" >> spaces
  (g, v) <- groupColParser
  _ <- spaces >> string "table-id:" >> spaces
  i <- identifierValue
  _ <- spaces
  t <- parseSectionText "table-title:"
  _ <- spaces >> string ":end-table:" >> readEol
  return LRSTable { tabId     = i
                  , tabGroup  = g
                  , tabVars   = v
                  , tabTitle  = t
                  }

chartTypeBar :: Parser LRSChartType
chartTypeBar = do
  _ <- string "bar-chart"
  return LRSChartBar

chartTypeLine :: Parser LRSChartType
chartTypeLine = do
  _ <- string "line-chart"
  return LRSChartLine

chartTypePie :: Parser LRSChartType
chartTypePie = do
  _ <- string "pie-chart"
  return LRSChartPie

logParseChart :: Parser LRSChart
logParseChart = do
  _ <- spaces >> string "define-chart:" >> spaces
  (g, v) <- groupColParser
  _ <- spaces >> string "chart-id:" >> spaces
  i <- identifierValue
  _ <- spaces
  t <- parseSectionText "chart-title:"
  _ <- spaces >> string "chart-use:" >> spaces
  r <- chartTypeBar <|> chartTypeLine <|> chartTypePie
  _ <- spaces >> string ":end-chart:" >> readEol
  return LRSChart { chartId     = i
                  , chartGroup  = g
                  , chartVars   = v
                  , chartType   = r
                  , chartTitle  = t
                  }

logParseStruct :: Parser LRSSpecifier
logParseStruct = do
  _ <- spaces >> char '$'
  i <- identifierValue
  _ <- char ':'
  v <- identifierValue
  _ <- readEol
  return LRSSpecifier { specName = i
                      , specVal  = v
                      }

logReportParser :: Parser LRSReport
logReportParser = do
  i <- parseSection "define-report:"
  t <- parseSectionText "define-title:"
  _ <- string "start-vars:"
  v <- many logParseVar
  _ <- string ":end-vars:"
  _ <- spaces >> string "start-tables:" >> spaces
  b <- many logParseTable
  _ <- spaces >> string ":end-tables:" >> spaces
  _ <- string "start-charts:" >> spaces
  c <- many logParseChart
  _ <- spaces >> string ":end-charts:" >> spaces
  _ <- string "report-structure:" >> spaces
  s <- many logParseStruct
  _ <- spaces >> string ":end-structure:"
  _ <- spaces >> string ":end-report:"
  _ <- spaces
  return LRSReport { reportId     = i
                   , reportTitle  = t
                   , reportVars   = v
                   , reportTables = b
                   , reportCharts = c
                   , reportSpec   = s
                   }

logSpecParser :: Parser LRS
logSpecParser = do
  _ <- spaces
  hdr <- logSpecHeader
  _ <- spaces >> string "start-rules:" >> spaces
  rules <- many logRuleParserParam
  _ <- spaces >> string ":end-rules:" >> spaces
  tokens <- logTokenizerParser
  _ <- spaces
  reports <- many logReportParser
  _ <- spaces
  return hdr { specRules   = rules
             , specTokens  = tokens
             , specReport  = reports
             }

parseLRS :: String -> Either String LRS
parseLRS s = let r = parse logSpecParser "[Invalid]" s
                     in case r of
                             Left  err -> Left $ show err
                             Right itm -> itm `seq` Right itm
