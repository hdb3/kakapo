{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Either(fromRight)
import Data.Maybe(fromMaybe,isJust,fromJust)
import Data.List (transpose)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Control.Applicative((<|>))
import System.Exit(die)
import System.Environment(getArgs)
import Paths(getFiles) -- getFiles :: [String] -> IO [String] -- a list of file names taken recursively from a list of paths

-- Compile time options:

-- what to do with well formed files
--action _ _ = return ()
--action f r = putStrLn f >> print r
action f (h,m,t) = putStrLn f >> print (expandDESCfield h, m, t)
--action f r = print $ check f r

-- whether to use the general parser (produces Text values)
-- or the format specific version (Int and Doubles)
parser = mfile -- custom, numeric
--parser = file -- Text only

-- continue after failure?
barf = putStrLn
--barf = die

main = do
   args <- getArgs
   if null args then do
       t <- T.getContents
       either (\s -> putStrLn $ "fail " ++ s) print (parseOnly parser t)
    else do
        names <- getFiles args
        mapM_ parseFile names
    where
        parseFile f = do
           t <- T.readFile f
           either (\s -> barf $ "failed to parse " ++ f ++ " (" ++ s ++ ")")  ( action f) (parseOnly parser t)

check :: String -> (Dict,Metrics,Dict) -> Either String String
check fname (ks,mx,_) = if cyclecount ks == length mx
                        then Right $ fname ++ " OK"
                        else Left $ fname ++ " failed check: " ++ show (cyclecount ks) ++ " /= " ++ show (length mx)
    where
        cyclecount = read . T.unpack . fromJust . lookup "CYCLECOUNT"

mfile :: Parser (Dict,Metrics,Dict)
mfile = name "mfile parser" $ do
    start <- singleLineSection "START"
    datas <- metrics
    stop <- singleLineSection "STOP"
    return (start, datas, stop)

type Metrics = [(Int,Double,Double,Double,Double)]
type Dict = [(Text,Text)]

metrics :: Parser Metrics
metrics = name "metrics" $ record "HDR" >> many' metric

metric = name "metric" $ do
    string "DATA"
    skipComma
    seq <- decimal
    skipComma
    d0 <- double
    skipComma
    d1 <- double
    skipComma
    d2 <- double
    skipComma
    d3 <- double
    skipSpace -- NOTE 'space' includes line end characters!!!
    return (seq,d0,d1,d2,d3)
 
eot :: Parser ()
eot = do
    m <- peekChar
    if isJust m then return() else fail "eot"
    
skipComma = Data.Attoparsec.Text.takeWhile (\c -> c == ',' || c == ' ')

isSpace c = c == '\t' || c == ' '
notSpace = not . isSpace
-- 'spaces' note is different to 'skipSpace'; it does not consume end-of-line
spaces = Data.Attoparsec.Text.takeWhile isSpace

records ::  Parser [[ Text ]]
records = anyFields `sepBy` char '\n'
anyFields :: Parser [ Text ]
anyFields = anyField `sepBy` char ','

anyField :: Parser Text
anyField = do
    spaces
    quotedField <|> plainField

quotedField :: Parser Text
quotedField = name "quotedField" $ do
    char quote
    takeTill isQuote <* char quote <* takeTill endOfField

plainField :: Parser Text
plainField = name "plainField" ( T.stripEnd <$> takeTill endOfField )

quote = '"'
isQuote = (quote ==)
endOfField c = c == '\n' || c == ','

field :: Text -> Parser Text
field t = string t <* takeTill endOfField

record :: Text -> Parser [ Text ]
record s = name ( "record " ++ T.unpack s) $ string s *> spaces *> char ',' *> anyFields <* Data.Attoparsec.Text.takeWhile ('\n'==)

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

-- key/value parsing for the START record DESC field

expandDESCfield :: [(Text,Text)] -> [(Text,Text)]
expandDESCfield = foldl p []
    where 
                                         -- fails only IF DESC present AND not parsable
                                         -- if this actually happens then should call directly as part of the
                                         -- main parser and allow the user to handle it with other parse errors
        p b a@(k,v) | k == "DESC" = b ++ fromRight [] ( parseOnly kvs v )
                    | otherwise = a : b

equals = '='
isEquals = (equals ==)

kvQuote = '\''
isKVquote = (kvQuote ==)

kvQuotedField = name "kvQuotedField" $ do
    char kvQuote *> takeTill isKVquote <* char kvQuote

kvField = name "kvField" ( Data.Attoparsec.Text.takeWhile notSpace )

kv = name "kv" $ do
    spaces
    key <- takeTill isEquals
    char equals
    value <- kvQuotedField <|> kvField
    return (key,value)

kvs :: Parser [(Text,Text)]
kvs = many' kv <* spaces
