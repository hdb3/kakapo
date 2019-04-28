{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Stages where
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe(fromJust)
import Data.Either(partitionEithers)
import Data.List(partition)
import Data.List.Extra(nubOn)
import Control.Monad(when)
import Control.Monad.Extra(concatMapM)
import System.Directory(listDirectory)
import System.Posix.Files(getFileStatus,isRegularFile,isDirectory,fileAccess)
import System.FilePath(combine)
import System.IO.Error(catchIOError)
import System.IO(stderr,hPutStrLn,hPrint)
import System.Environment(getArgs)
import Sections(Section,getSections,addToHeader,addHASH)
import Summarise(Point,DataPoint,processSection)

stageOne :: [String] -> IO [String]
-- the output is guaranteed to be names of regular files
stageOne = concatMapM stageOneA
    where
    stageOneA :: String -> IO [String]
    -- if it is a regular file, return itself
    -- if it is a directory, use itself recursively
    stageOneA path = do
        status <- getFileStatus path
        if isRegularFile status then
            return [ path ]
        else if isDirectory status then
            map (combine path) <$> listDirectory path >>= concatMapM stageOneA 
        else return []

stageTwo :: String -> IO (String,Text)
-- read files and deliver contents paired with name
-- silently pass over inaccessible and invalid Text content
-- TODO make return type Either and warn about read failures (use tryIOError)
stageTwo path = do
    isReadable <- fileAccess path True False False
    if isReadable
    then do
        content <- catchIOError ( T.readFile path )
                                (\_ -> return T.empty) 
        return (path,content)
    else return (path,T.empty)

stageThree :: (String,Text) -> (String,[Section])
-- stageThree does the textual parsing for fields based on comma seprators and quote marks
-- type Section is a triple of Text maps:
-- type Section = ( [(Text,Text)] , [(Text,[Text])] , [(Text,Text)] )
-- note: main (Data) element is a list with multiple members consisting of original raw data still in textual format
--       we can assume (and check) that the data is numeric, though
stageThree (path,content) = (path, getSections content)

stageFour :: (String,[Section]) -> [Section]
-- NOTE: adding the HASH _before_ adding the source so that copied file content is properly identified as equivalent
stageFour (path,sections) = map ((addToHeader ("SOURCE", T.pack path)) . addHASH) sections

data HeaderRecV1 = HeaderRecV1 { hrV1HASH , hrV1PID , hrV1BLOCKSIZE , hrV1GROUPSIZE , hrV1MAXBURSTCOUNT , hrV1CYCLECOUNT , hrV1CYCLEDELAY :: Int
                               , hrV1DESC, hrV1START, hrV1STOP, hrV1SOURCE :: Text
                               } deriving (Show, Eq)

data GKRecV1 a = GKRecV1 { hrec :: HeaderRecV1, values :: a } deriving (Show, Eq)
newtype KRecV1 = KRecV1 { kRecV1 :: (GKRecV1 [(Text, [Text])]) } deriving (Show, Eq)
krecHeader = hrec . kRecV1
krecValues = values . kRecV1
krecValue k = ( lookup k ) . krecValues

mapCheck :: [Text] -> [(Text,t)] -> Bool
mapCheck keys kvs = foldr ((&&) . check) True keys
    where check a = a `elem` map fst kvs

doCycleCheck krecs = do
   let (complete,incomplete) = partition cycleCheck krecs
   when (0 /= length incomplete)
        ( do hPutStrLn stderr $ "complete/incomplete count: " ++ show (length complete, length incomplete)
             mapM_ print $ map (\krec -> (cycleComp krec , krec)) incomplete )
   return complete
    where

        cycleCheck :: KRecV1 -> Bool
        cycleCheck = (\(a,b) -> a == b) . cycleComp

        cycleComp :: KRecV1 -> (Int,Int)
        cycleComp  (KRecV1 GKRecV1{..}) = ( length $ fromJust $ lookup "SEQ" values , hrV1CYCLECOUNT hrec)

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

        --valueCheckV1 = mapCheck valueKeysV1
        --valueKeysV1 = [ "RTT" , "LATENCY" , "TXDURATION", "RXDURATION" ]
        --valuesFail = not $ valueCheckV1 values
        --lookupValues k = fromJust $ lookup k values
        --kv1RTT  = lookupValues "RTT"
        --kv1LATENCY = lookupValues "LATENCY"
        --kv1TXDURATION = lookupValues "TXDURATION"
        --kv1RXDURATION = lookupValues "RXDURATION"

    in if headerFail then Left $ "headerFail: " ++ show header 
       --else if valuesFail then Left $ "valuesFail: " ++ show (header, values)
       else Right $ GKRecV1 hrec values

getKRecV1 :: IO ([String], [KRecV1])
getKRecV1 = do
   getArgs >>= getKRecV1_

getKRecV1_ :: [String] -> IO ([String], [KRecV1])
getKRecV1_ dirs = do
   files <- stageOne dirs
   hPutStrLn stderr $ "getKRecV1: read " ++ show ( length files ) ++ " files"
   contents <- mapM stageTwo files
   let dataPoints = concatMap ( stageFour . stageThree ) contents
       (errors,krecs)  = partitionEithers $ map stageFive dataPoints
   hPutStrLn stderr $ "getKRecV1: read " ++ show ( length krecs ) ++ " krecs"
   hPutStrLn stderr $ "getKRecV1: found " ++ show ( length errors ) ++ " errors"
   let uniqKRecs = nubOn ( hrV1HASH . hrec ) krecs -- :: [ KRecV1 ]
   hPutStrLn stderr $ "getKRecV1: unique " ++ show ( length uniqKRecs ) ++ " uniqKRecs"
   completeKRecs <- doCycleCheck $ map KRecV1 uniqKRecs
   return (errors, completeKRecs)

main = do
   (errors,krecs) <- getKRecV1
   --mapM_ print dataPoints
   --mapM_ print krecs
   mapM_ print errors
   hPutStrLn stderr "Done"
