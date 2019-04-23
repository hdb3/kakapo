module Stages where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Extra(concatMapM)
import System.Directory(listDirectory)
import System.Posix.Files(getFileStatus,isRegularFile,isDirectory,fileAccess)
import System.FilePath(combine)
import System.IO.Error(catchIOError)
import System.Environment(getArgs)

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

main = do
   args <- getArgs
   files <- stageOne args
   putStrLn $ unlines files
   contents <- mapM stageTwo files
   let lengths = map (\(p,t) -> (p, T.length t)) contents
   print lengths

{-
stageThree :: String -> Either String [String] -- the Right outputs are guaranteed to be well formed sections - Left outputs are helpful error messages
stageFour :: String -> ([String,String],[String,[Double]]) -- we have read all of the headers and parsed all of the data.
stageFive :: [Double] -> (Double,Double) -- compute means and deviations
stageSix :: ([Source, ([String,String],[String,(Double,Double)]) -> [(Int,[String],[String])] -- report metadata classes, with frequency.
stageSeven :: Dataset -> [([String],([String],[Double]))] -- partition the dataset based on the discovered classes

-}
