module Main where
import System.Process
import System.Posix.User
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe(fromMaybe)
import Text.Read(readMaybe)
import System.IO(hClose,hPutStrLn,openFile,IOMode(WriteMode))

main = do
    euid <- getEffectiveUserID
    if euid == 0 then do
        memsize <- getMemSize
        (cores,cpus) <- getCPUcounts
        putStrLn $ "MEMSIZE=" ++ show memsize
        putStrLn $ "CORES=" ++ show cores
        putStrLn $ "THREADS=" ++ show cpus
        --runfile <- openFile "sysinfo.txt" WriteMode
        --runfile <- openFile "/run/sysinfo" WriteMode
        runfile <- openFile "/var/run/sysinfo" WriteMode
        hPutStrLn runfile $ "MEMSIZE=" ++ show memsize
        hPutStrLn runfile $ "CORES=" ++ show cores
        hPutStrLn runfile $ "THREADS=" ++ show cpus
        hClose runfile
    else
        putStrLn "Error - root privelege required"

getMemSize :: IO Int
getMemSize = do
    dmidecode17 <- readProcess "dmidecode" ["--type=17" ] ""
    return $ (sum . map (safeRead . getMemSizeStrings) . filter isSizeDescriptor . lines) dmidecode17
    where
    safeRead s = fromMaybe 0 (readMaybe s)
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
