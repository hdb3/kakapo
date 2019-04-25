module LogFileNames where
import System.IO
import System.Directory
import Data.List(isPrefixOf,sort)

{-
Required: a way to reliablly get the next file index name, when there are very many files.
Solution: look for dir entries for the ‘cmd’ file, using System.Directory(listDirectory).
Filter on the prefix without numeric tail.
Sort the candidates.
Get the last.
Cut the prefix.
Read the remainder.
Add 1.
Generate the new postfix.
-}

getNextFileBaseName :: String -> String -> IO String
-- The input is _exactly_ the name wanted, less the extension of format .NNNN
-- The response is _just_ the wanted extension
-- In simple case the caller would just join the input with the output to get the next file
-- This function does not attempt to check if such a file exists or could be created....
--getNextFileBaseName _ = return ".0000"
getNextFileBaseName cwd root = do
    dir <- listDirectory cwd
    let matches = filter (isPrefixOf (root ++ ".")) dir
    -- ideally should filter out any non-numeric postfixes....
    -- cheat by converting to numeric before sorting
    if null matches then do
        return $ "." ++ ext 0 
    else do
        let matches' = map ( read . drop (1 + length root)) matches :: [Int]
        let latest = last $ sort matches'
        let next = 1 + latest
        return $ "." ++ ext next 
    where
        ext n = pad 4 $ show n
        pad n s | length s >= n = s
                | otherwise = pad n $ "0" ++ s

touch :: String -> IO()
touch p = openFile p WriteMode >>= hClose
