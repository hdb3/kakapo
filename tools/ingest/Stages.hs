{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Stages where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe(fromJust)
import Control.Monad.Extra(concatMapM)
import System.Directory(listDirectory)
import System.Posix.Files(getFileStatus,isRegularFile,isDirectory,fileAccess)
import System.FilePath(combine)
import System.IO.Error(catchIOError)
import System.Environment(getArgs)
import Sections(Section,getSections,addToHeader)
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
-- TODO make retrun type Either and warn about read failures (use tryIOError)
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

-- declarations from Summarise / Sections:
-- type Point = (Int,Double,Double,Double)
-- type DataPoint = ( [(T.Text , T.Text) ] , [( T.Text , Point) ])
-- processSection :: Section -> DataPoint

--stageFour :: (String,[Section]) -> (String,[DataPoint])
-- stageFour (path,sections) = (path, map processSection sections)
stageFour :: (String,[Section]) -> [DataPoint]
stageFour (path,sections) = map ( processSection . addToHeader ("SOURCE" ,T.pack path)) sections


data KRecV1 = KRecV1 { kV1PID :: Int
                     , kV1DESC, kV1START :: T.Text
                     , kV1BLOCKSIZE , kv1GROUPSIZE , kv1MAXBURSTCOUNT , kv1CYCLECOUNT , kv1CYCLEDELAY :: Int
                     , kv1RTT , kv1LATENCY , kv1TXDURATION , kv1RXDURATION :: Point
                     , kv1SOURCE :: T.Text
                     } deriving (Show, Eq)

stageFive :: DataPoint -> KRecV1
stageFive ( header , values ) =
    let
        lookupHeader k = fromJust $ lookup k header
        lookupValues k = fromJust $ lookup k values
        kv1SOURCE = lookupHeader "SOURCE"
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
    in KRecV1 {..}

main = do
   args <- getArgs
   files <- stageOne args
   --putStrLn $ unlines files
   contents <- mapM stageTwo files
   let lengths = map (\(p,t) -> (p, T.length t)) contents
   --print lengths
   let sections = map stageThree contents
       allSections = concatMap snd sections
   putStrLn $ "read " ++ show ( length files ) ++ " files and " ++ show ( length allSections ) ++ " sections"
   let dataPoints = concatMap ( stageFour . stageThree ) contents
       krecs  = map stageFive dataPoints
   --mapM_ print dataPoints
   mapM_ print krecs

