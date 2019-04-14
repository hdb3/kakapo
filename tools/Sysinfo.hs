module Main where
import System.Process
import System.Posix.User
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main = do
    euid <- getEffectiveUserID
    if (euid == 0) then do
        dmidecode17 <- readProcess "dmidecode" ["--type=17" ] ""
        let memsize = getMemSize dmidecode17
        putStrLn $ "memsize is " ++ show memsize ++ "MB"
    else
        putStrLn "Error - root privelege required"

getMemSize :: String -> Int
getMemSize = sum . map (read . getMemSizeStrings) . filter isSizeDescriptor . lines
    where
    isSizeDescriptor = isPrefixOf "Size:" . dropWhile isSpace
    getMemSizeStrings :: String -> String
    getMemSizeStrings = (!! 1) . words
