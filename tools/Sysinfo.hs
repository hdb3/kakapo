module Main where
import System.Process
import System.Posix.User
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main = do
    euid <- getEffectiveUserID
    if (euid == 0) then do
        memsize <- getMemSize
        putStrLn $ "memsize is " ++ show memsize ++ " MB"
        (cores,cpus) <- getCPUcounts
        putStrLn $ "core/cpus is " ++ show (cores,cpus)
    else
        putStrLn "Error - root privelege required"

getMemSize :: IO Int
getMemSize = do
    dmidecode17 <- readProcess "dmidecode" ["--type=17" ] ""
    return $ (sum . map (read . getMemSizeStrings) . filter isSizeDescriptor . lines) dmidecode17
    where
    isSizeDescriptor = isPrefixOf "Size:" . dropWhile isSpace
    getMemSizeStrings :: String -> String
    getMemSizeStrings = (!! 1) . words

getCPUcounts :: IO (Int,Int)
getCPUcounts = do
    lscpuOutput <- readProcess "lscpu" [] ""
    let
        getFirstMatchingKey :: [(String,String)] -> String -> Int
        getFirstMatchingKey kvx k = ( read . snd . head . filter ( (k ==) . fst )) kvx 

        parselscpuLine :: String -> (String,String)
        parselscpuLine s = (key,value)
            where
            key = takeWhile (':' /=) s
            value = dropWhile isSpace $ dropWhile (':' ==) $ dropWhile (':' /=) s

        kvs = map parselscpuLine $ lines lscpuOutput
        threads = getFirstMatchingKey kvs "CPU(s)"
        sockets = getFirstMatchingKey kvs "Socket(s)"
        coresPerSocket = getFirstMatchingKey kvs "Core(s) per socket"
        cores = sockets * coresPerSocket
    return (cores,threads)
