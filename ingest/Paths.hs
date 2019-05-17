module Paths where
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Extra(concatMapM)
import System.Directory(listDirectory)
import System.Posix.Files(getFileStatus,isRegularFile,isDirectory,fileAccess)
import System.FilePath(combine)
import System.IO.Error(catchIOError)

getFiles :: [String] -> IO [String]
-- the output is guaranteed to be names of regular files
getFiles = concatMapM getFiles'
    where
    getFiles' :: String -> IO [String]
    -- if it is a regular file, return itself
    -- if it is a directory, use itself recursively
    getFiles' path = do
        status <- getFileStatus path
        if isRegularFile status then
            return [ path ]
        else if isDirectory status then
            map (combine path) <$> listDirectory path >>= concatMapM getFiles' 
        else return []

getContent :: String -> IO (String,Text)
-- read files and deliver contents paired with name
-- silently pass over inaccessible and invalid Text content
-- TODO make return type Either and warn about read failures (use tryIOError)
getContent path = do
    isReadable <- fileAccess path True False False
    if isReadable
    then do
        content <- catchIOError ( T.readFile path )
                                (\_ -> return T.empty) 
        return (path,content)
    else return (path,T.empty)
