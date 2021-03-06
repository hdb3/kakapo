{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Stages where
import Data.Text(Text)
import qualified Data.Text as T
import Data.Maybe(fromJust,fromMaybe)
import Data.Either(partitionEithers)
import Data.List(partition)
import Data.List.Extra(nubOn)
import Control.Monad(when)
import System.IO(stderr,hPutStrLn)
import System.Environment(getArgs)
import Sections(Section,getSections,addToHeader,addHASH)
import Paths

stageOne :: [String] -> IO [String]
-- the output is guaranteed to be names of regular files
stageOne = getFiles
stageTwo :: String -> IO (String,Text)
-- read files and deliver contents paired with name
-- silently pass over inaccessible and invalid Text content
stageTwo = getContent

stageThree :: (String,Text) -> (String,[Section])
-- stageThree does the textual parsing for fields based on comma seprators and quote marks
-- type Section is a triple of Text maps:
-- type Section = ( [(Text,Text)] , [(Text,[Text])] , [(Text,Text)] )
-- note: main (Data) element is a list with multiple members consisting of original raw data still in textual format
--       we can assume (and check) that the data is numeric, though
stageThree (path,content) = (path, getSections content)

stageFour :: (String,[Section]) -> [Section]
-- NOTE: adding the HASH _before_ adding the source so that copied file content is properly identified as equivalent
stageFour (path,sections) = map (addToHeader ("SOURCE", T.pack path) . addHASH) sections

data HeaderRecV1 = HeaderRecV1 { hrV1HASH , hrV1PID , hrV1BLOCKSIZE , hrV1GROUPSIZE , hrV1MAXBURSTCOUNT , hrV1CYCLECOUNT , hrV1CYCLEDELAY :: Int
                               , hrV1DESC, hrV1START, hrV1STOP, hrV1SOURCE :: Text
                               } deriving (Show, Eq)

data GKRecV1 a = GKRecV1 { hrec :: HeaderRecV1, values :: a } deriving (Show, Eq)
newtype KRecV1 = KRecV1 { kRecV1 :: GKRecV1 [(Text, [Text])] } deriving (Show, Eq)

krecHeader :: KRecV1 -> HeaderRecV1
krecHeader = hrec . kRecV1

krecValues :: KRecV1 -> [(Text, [Text])]
krecValues = values . kRecV1

mapCheck :: [Text] -> [(Text,t)] -> Bool
mapCheck keys kvs = foldr ((&&) . check) True keys
    where check a = a `elem` map fst kvs

doCycleCheck :: [KRecV1] -> IO [KRecV1]
doCycleCheck krecs = do
   let (complete,incomplete) = partition cycleCheck krecs
   when (0 /= length incomplete)
        ( do hPutStrLn stderr $ "complete/incomplete count: " ++ show (length complete, length incomplete)
             -- this may be useful on occasion, but rarely, so commented in the absence of a verbose flag
             --mapM_ print $ map (\krec -> (cycleComp krec , krec)) incomplete
             mapM_ (print . hrV1SOURCE . krecHeader) incomplete
        )
   return complete
    where

        cycleCheck :: KRecV1 -> Bool
        cycleCheck = uncurry (==) . cycleComp

        cycleComp :: KRecV1 -> (Int,Int)
        cycleComp  (KRecV1 GKRecV1{..}) = ( length $ lookupSEQ values , hrV1CYCLECOUNT hrec)
            where 
                -- 'values' is a list of columns an dheaders - we look for the SEQ columns merely becuase it should always be present
                --     however, all we want is to know the length of any column, as they are all equal length
                lookupSEQ vals = fromMaybe [] ( lookup "SEQ" vals)

stageFive :: ( [(T.Text , T.Text) ] , a ) -> Either String ( GKRecV1 a)
stageFive (header,values) =
    let
        lookupHeaderInt :: Text -> Int
        lookupHeaderInt = read . T.unpack . lookupHeader
        headerCheckV1 = mapCheck [ "SOURCE", "HASH" , "PID" , "DESC" , "START", "STOP", "BLOCKSIZE", "GROUPSIZE", "MAXBURSTCOUNT", "CYCLECOUNT", "CYCLEDELAY" ]
        headerFail = not $ headerCheckV1 header
        lookupHeader k = fromJust $ lookup k header

        hrV1SOURCE = lookupHeader "SOURCE"
        hrV1DESC = lookupHeader "DESC"
        hrV1START  = lookupHeader "START"
        hrV1STOP  = lookupHeader "STOP"
        hrV1PID = lookupHeaderInt "PID"
        hrV1HASH  = lookupHeaderInt "HASH"
        hrV1BLOCKSIZE  = lookupHeaderInt "BLOCKSIZE"
        hrV1GROUPSIZE  = lookupHeaderInt "GROUPSIZE"
        hrV1MAXBURSTCOUNT  = lookupHeaderInt "MAXBURSTCOUNT"
        hrV1CYCLECOUNT  = lookupHeaderInt "CYCLECOUNT"
        hrV1CYCLEDELAY  = lookupHeaderInt "CYCLEDELAY"
        hrec = HeaderRecV1 {..}

    --in if headerFail then Left $ "headerFail: " ++ show header 
    in if headerFail then Left $ "headerFail: " ++ ( if mapCheck ["SOURCE"] header then T.unpack hrV1SOURCE else "missing source in header!!!" ++ show header )
       else Right $ GKRecV1 hrec values

getKRecV1 :: IO ([String], [KRecV1])
getKRecV1 = getArgs >>= getKRecV1_

getKRecV1_ :: [String] -> IO ([String], [KRecV1])
getKRecV1_ dirs = do
   files <- stageOne dirs
   hPutStrLn stderr $ "getKRecV1: read " ++ show ( length files ) ++ " files"
   contents <- mapM stageTwo files
   let dataPoints = concatMap ( stageFour . stageThree ) contents
       (errors,krecs)  = partitionEithers $ map stageFive dataPoints
   hPutStrLn stderr $ "getKRecV1: read " ++ show ( length krecs ) ++ " krecs"
   hPutStrLn stderr $ "getKRecV1: found " ++ show ( length errors ) ++ " errors"
   let uniqKRecs = nubOn ( hrV1HASH . hrec ) krecs
   hPutStrLn stderr $ "getKRecV1: unique " ++ show ( length uniqKRecs ) ++ " uniqKRecs"
   completeKRecs <- doCycleCheck $ map KRecV1 uniqKRecs
   return (errors, completeKRecs)

main :: IO ()
main = do
   (errors,krecs) <- getKRecV1
   mapM_ print errors
   hPutStrLn stderr $ "Done, " ++ show (length krecs) ++ " records found" 
