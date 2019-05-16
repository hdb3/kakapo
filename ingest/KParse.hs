{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Maybe(fromMaybe,isJust)
import Data.List (transpose)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Control.Applicative((<|>))
import System.Exit(die)
import System.Environment(getArgs)
import KakapoData

-- Compile time options:

-- what to do with well formed files
--action _ _ = return ()
--action f r = putStrLn f >> print r
action f r = print $ check f r

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
    else
        mapM_ parseFile args
    where
        parseFile f = do
           t <- T.readFile f
           either (\s -> barf $ "failed to parse " ++ f ++ " (" ++ s ++ ")")  ( action f) (parseOnly parser t)

check :: String -> (KakapoStart,Metrics,Dict) -> Either String String
check fname (ks,mx,_) = if cyclecount ks == length mx
                        then Right $ fname ++ " OK"
                        else Left $ fname ++ " failed check: " ++ show (cyclecount ks) ++ " /= " ++ show (length mx)

mfile :: Parser (KakapoStart,Metrics,Dict)
mfile = name "mfile parser" $ do
    start <- Main.start
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
-- note is different to skipSpace; does not consume end-of-line
spaces = Data.Attoparsec.Text.takeWhile (\c -> c == '\t' || c == ' ')

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

-- Parse specific START records which have this format:
-- HDR , PID , DESC , START , BLOCKSIZE, GROUPSIZE, MAXBURSTCOUNT, CYCLECOUNT, CYCLEDELAY
-- START, 8, "TOPIC='EXPBSVLARGE'  PLATFORM=frr SUT=docker02  TIME=1557971107 UUID=3baffac7-777c-11e9-8001-509a4c46ef1c VERSION='bgpd version 7.0' MEMSIZE=16384 CORES=4 THREADS=4 " , "Thu May 16 02:05:47 2019" , 1584, 100, 1, 20, 0

start :: Parser KakapoStart
start = startHeader >> startBody
startHeader = name "startHeader" $ string "HDR , PID , DESC , START , BLOCKSIZE, GROUPSIZE, MAXBURSTCOUNT, CYCLECOUNT, CYCLEDELAY" >> char '\n'
startBody :: Parser KakapoStart
startBody = name "startBody" $ do
    name "START" $ string "START"
    skipComma
    pid <- decimal
    skipComma
    name "q1" $ char quote
    string "TOPIC"
    topic <- quoted
    string "PLATFORM"
    platform <- unquoted
    string "SUT"
    sut <- unquoted
    string "TIME"
    time <- decimal_
    string "UUID"
    uuid <- unquoted
    string "VERSION"
    version <- quoted
    string "MEMSIZE"
    memsize <- decimal_
    string "CORES"
    cores <- decimal_
    string "THREADS"
    threads <- decimal_
    name "q2" $ char quote
    skipComma
    start <- quotedField
    skipComma
    blocksize <- decimal
    skipComma
    groupsize <- decimal
    skipComma
    maxburstcount <- decimal
    skipComma
    cyclecount <- decimal
    skipComma
    cycledelay <- decimal
    skipSpace
    return KakapoStart{..}

quoted = name "quoted" $ string "='" >> takeTill ('\''==) <* "'" <* spaces
unquoted = name "unquoted" $ char '=' >> takeTill (\c -> c == ' ' || c == '"') <* spaces
decimal_ = name "decimal_" $ char '=' >> decimal <* spaces
