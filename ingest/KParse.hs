{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (transpose)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Control.Applicative((<|>))
import System.Exit(die)
import System.Environment(getArgs)

main = do
   args <- getArgs
   if null args then do
       t <- T.getContents
       either (\s -> putStrLn $ "fail " ++ s) print (parseOnly file t)
    else
        mapM_ parseFile args

parseFile f = do
   t <- T.readFile f
   either (\s -> putStrLn $ "failed to parse " ++ f ++ " (" ++ s ++ ")")  (\a -> return () ) (parseOnly file t)
   --either (\s -> die $ "failed to parse " ++ f ++ " (" ++ s ++ ")")  (\a -> return () ) (parseOnly file t)
   
records ::  Parser [[ Text ]]
records = anyFields `sepBy` char '\n'
anyFields :: Parser [ Text ]
anyFields = anyField `sepBy` char ','

anyField :: Parser Text
anyField = do
    skipSpace
    quotedField <|> plainField

quotedField :: Parser Text
quotedField = do
    char quote
    takeTill isQuote <* char quote <* takeTill endOfField

plainField :: Parser Text
plainField = T.stripEnd <$> takeTill endOfField

quote = '"'
isQuote = (quote ==)
endOfField c = c == '\n' || c == ','

field :: Text -> Parser Text
field t = string t <* takeTill endOfField

record :: Text -> Parser [ Text ]
record s = name ( "record " ++ T.unpack s) $ string s *> skipSpace *> char ',' *> anyFields <* Data.Attoparsec.Text.takeWhile ('\n'==)

{- -- SIMPLE EXAMPLE USAGE 
section = do
    header *> start *> header *> datas <* header <* stop
    where

        header = record "HDR" <?> "HDR"
        start = record "START" <?> "START"
        stop = record "STOP" <?> "STOP"
        data_ = record "DATA" <?> "DATA"
        datas = many1 data_
-}

name = flip (<?>)

singleLineSection :: Text -> Parser [(Text,Text)]
singleLineSection l = name "singleLineSection parser" $ do
    keys <- record "HDR"
    values <- record l
    return $ zip keys values

multiLineSection :: Text -> Parser [(Text,[Text])]
multiLineSection l = name "multiLineSection parser" $ do
    keys <- record "HDR"
    values <- many' (record l)
    return $ zip keys (transpose values)

file :: Parser ( [(Text,Text)] , [(Text,[Text])], [(Text,Text)])

file = name "file parser" $ do
    start <- singleLineSection "START"
    datas <- multiLineSection "DATA"
    stop <- singleLineSection "STOP"
    return (start, datas, stop)
