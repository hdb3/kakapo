{-# LANGUAGE OverloadedStrings #-}
module GenParse where

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

type Metrics = [(Int,Double,Double,Double,Double)]
type Dict = [(Text,Text)]
type Sample = (Dict, Metrics)
type Samples = [Sample]

rtt :: Metrics -> [ Double ]
rtt = map (\(_,x,_,_,_) -> x)

getData :: IO [Either String (Dict, Metrics)]
getData = do
   args <- getArgs
   if null args then
       die "please specify a path to search for kakapo data files"
    else do
        names <- getFiles [ head args ]
        mapM parseFile names
    where
        expandDESCfields fname ( Right (h,m,t)) = let size = length m in if size == 0 then Left $ "Parse failed size check in " ++ fname else Right (("SIZE",T.pack $ show size) : ("SOURCE",T.pack fname) : expandDESCfield h, m)
        --expandDESCfields _ ( Right (h,m,t)) = Right (expandDESCfield h, m)
        expandDESCfields fname ( Left s ) = Left $ "Parse fail in " ++ fname ++ " : " ++ s 
        parseFile f = do
           t <- T.readFile f
           return $ expandDESCfields f $ parseOnly mfile t

-- not used but perhaps should be unless we export the filename
check :: String -> (Dict,Metrics) -> Either String String
check fname (ks,mx) = if cyclecount ks == length mx
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

kvQuotedField = name "kvQuotedField" $ char kvQuote *> takeTill isKVquote <* char kvQuote

kvField = name "kvField" ( Data.Attoparsec.Text.takeWhile notSpace )

kv = name "kv" $ do
    spaces
    key <- takeTill isEquals
    char equals
    value <- kvQuotedField <|> kvField
    return (key,value)

kvs :: Parser [(Text,Text)]
kvs = many' kv <* spaces
