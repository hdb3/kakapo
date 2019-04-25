{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Stages where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe(fromJust)
import Data.Either(partitionEithers)
import Data.List.Extra(nubOn)
import Control.Monad.Extra(concatMapM)
import System.Directory(listDirectory)
import System.Posix.Files(getFileStatus,isRegularFile,isDirectory,fileAccess)
import System.FilePath(combine)
import System.IO.Error(catchIOError)
import System.IO(stderr,hPutStrLn)
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

stageTwo :: String -> IO (String,T.Text)
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

stageThree :: (String,T.Text) -> (String,[Section])
stageThree (path,content) = (path, getSections content)

stageFour :: (String,[Section]) -> [DataPoint]
-- NOTE: adding the HASH _before_ adding the source so that copied file content is properly identified as equivalent
stageFour (path,sections) = map ((processSection . addToHeader ("SOURCE", T.pack path)) . addHASH) sections

data KRecV1 = KRecV1 { kV1HASH :: Int
                     , kv1SOURCE :: T.Text
                     , kV1PID :: Int
                     , kV1DESC, kV1START :: T.Text
                     , kV1BLOCKSIZE , kv1GROUPSIZE , kv1MAXBURSTCOUNT , kv1CYCLECOUNT , kv1CYCLEDELAY :: Int
                     , kv1RTT , kv1LATENCY , kv1TXDURATION , kv1RXDURATION :: Point
                     } deriving (Show, Eq)


mapCheck :: [T.Text] -> [(T.Text,t)] -> Bool
mapCheck keys kvs = foldr ((&&) . check) True keys
    where check a = a `elem` map fst kvs

{-
mapCheck' keys kvs = go keys
    where check a = a `elem` map fst kvs
          go = foldr ((&&) . check) True
-}

headerCheckV1 = mapCheck headerKeysV1
headerKeysV1 = [ "SOURCE", "HASH" , "PID" , "DESC" , "START", "BLOCKSIZE", "GROUPSIZE", "MAXBURSTCOUNT", "CYCLECOUNT", "CYCLEDELAY" ]

valueCheckV1 = mapCheck valueKeysV1
valueKeysV1 = [ "RTT" , "LATENCY" , "TXDURATION", "RXDURATION" ]

stageFive :: DataPoint -> Either String KRecV1
stageFive ( header , values ) =
    let
        headerFail = not $ headerCheckV1 header
        valuesFail = not $ valueCheckV1 values
        lookupHeader k = fromJust $ lookup k header
        lookupValues k = fromJust $ lookup k values
        kv1SOURCE = lookupHeader "SOURCE"
        kV1HASH  = read $ T.unpack $ lookupHeader "HASH"
        kV1PID = read $ T.unpack $ lookupHeader "PID"
        kV1DESC = lookupHeader "DESC"
        kV1START  = lookupHeader "START"
        kV1BLOCKSIZE  = read $ T.unpack $ lookupHeader "BLOCKSIZE"
        kv1GROUPSIZE  = read $ T.unpack $ lookupHeader "GROUPSIZE"
        kv1MAXBURSTCOUNT  = read $ T.unpack $ lookupHeader "MAXBURSTCOUNT"
        kv1CYCLECOUNT  = read $ T.unpack $ lookupHeader "CYCLECOUNT"
        kv1CYCLEDELAY  = read $ T.unpack $ lookupHeader "CYCLEDELAY"
        kv1RTT  = lookupValues "RTT"
        kv1LATENCY = lookupValues "LATENCY"
        kv1TXDURATION = lookupValues "TXDURATION"
        kv1RXDURATION = lookupValues "RXDURATION"
    in if headerFail then Left $ "headerFail: " ++ show header 
       else if valuesFail then Left $ "valuesFail: " ++ show (header, values)
       else Right KRecV1 {..}

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
   let uniqKRecs = nubOn kV1HASH krecs :: [ KRecV1 ]
   hPutStrLn stderr $ "getKRecV1: unique " ++ show ( length uniqKRecs ) ++ " uniqKRecs"
   return (errors, uniqKRecs)

main = do
   (errors,krecs) <- getKRecV1
   --mapM_ print dataPoints
   --mapM_ print krecs
   mapM_ print errors
   hPutStrLn stderr "Done"
