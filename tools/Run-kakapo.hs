module Main where
import System.IO(stderr,hPutStrLn,hPrint)
import Data.Maybe(fromJust,isJust)
import System.Environment(getArgs)
import Data.List(intercalate,lookup)
import System.Exit
import Data.Char(isControl)
import Runner

localhost = ( "root@localhost" , "root@localhost" )
docker02 = ( "root@docker02" , "root@10.30.65.214" )

frr = ( "hdb3/frr", "frr")
bird = ("hdb3/bird","bird")

presetTargets = [("localhost",localhost) , ("docker02",docker02) ]
presetPlatforms = [("bird",bird),("frr",frr)]

main = do
    args <- getArgs
    if 4 == length args then do
        hPutStrLn stderr "using raw arguments from command line"
        start (args !! 0 , args !!1 ) (args !! 2 , args !!3 ) 
    else if 2 == length args then do
        let target = lookup ( args !! 0 ) presetTargets
            platform = lookup ( args !! 1 ) presetPlatforms
        if isJust platform && isJust target then do
            hPutStrLn stderr "using predefined arguments based on command line labels"
            start (fromJust target) (fromJust platform)
        else do
            hPutStrLn stderr "could not find presets"
            hPrint stderr presetTargets
            hPrint stderr presetPlatforms
    else
        hPutStrLn stderr "please specify target systems and platforms"

start :: (String , String ) -> ( String , String ) -> IO ()
start ( sutHostName , kakapoHostName) (repo , app ) = do
    let
        sut = ssh [ sutHostName ]
        getSUT = getSSH [ sutHostName ]

        kakapo = ssh [ "-p" , "65535" , kakapoHostName ]

        kakapoHost = ssh [ kakapoHostName ]

    sut $ "docker kill " ++ app
    sut $ "docker pull " ++ repo
    sysinfo' <- getSUT $ "docker run --name " ++ app ++ " --privileged -t --rm " ++ repo ++ " sysinfo"
    let sysinfo = maybe "VERSION=\"UNKNOWN\" MEMSIZE=-1 CORES=-1 THREADS=-1" (map (\c -> if isControl c then ' ' else c)) sysinfo'
    putStrLn $ "Sysinfo: " ++ sysinfo

    sut $ "docker run --name " ++ app ++ " --privileged -d --network host --rm " ++ repo

    kakapoHost "docker kill kakapo"
    kakapoHost "docker kill kakapo ; docker pull hdb3/kakapo ; docker run --name kakapo --privileged --rm -d --network host --hostname kakapo hdb3/kakapo"

    runExperiment kakapo sysinfo app

    sut $ "docker kill " ++ app
    kakapoHost "docker kill kakapo"

    putStrLn "Done"

runExperiment :: ([Char] -> IO()) -> String -> String -> IO()
runExperiment rsh logtext app = do
    let
        base = kvSet "LOGPATH" ( "10.30.65.209/" ++ app ) $ kvSet "LOGTEXT" ( "\"" ++ logtext ++ "\"") kakapoDefaultParameters
        gsr = [1..10]
        gsrx = [10,20..50]
        bsr = [1..10] ++ [10,20..100] ++ [100,200..1000] ++ [1000,2000..10000] ++ [10000,20000..100000]
        bsr0 = [1..10]
        bsrx = [100000,200000..1000000]

        genCommands :: String -> [[(String,String)]] -> String -> [String]
        genCommands pre parameterLists post = map (\parameterList -> intercalate " " [pre, expandParameters parameterList, post ]) parameterLists
        expandParameters = intercalate " " . map (\(k,v) -> k ++ "=" ++ v)
        kakapoDefaultParameters =
           [
             ("LOGTEXT" , "\"LOGTEXT not provided\" "),
             ("LOGPATH" , "10.30.65.209"),
             ("SLEEP" , "10 "),
             ("MAXBURSTCOUNT" , "1 "),
             ("GROUPSIZE" , "10 "),
             ("BLOCKSIZE" , "1 "),
             ("CYCLEDELAY" , "0 "),
             ("CYCLECOUNT" , "10 "),
             ("NEXTHOP" , "169.254.0.11 ")
           ]

        kvSet k v = map (\(a,b) -> if a == k then (a,v) else (a,b) )

        kvGen k vx m = map (\v -> kvSet k ( show v) m) vx

        blockGen bsRange gsRange base = [ expandParameters $ kvSet "GROUPSIZE" ( show gs ) $ kvSet "BLOCKSIZE" ( show bs ) base | bs <- bsRange , gs <- gsRange ]

        buildCommand parameters = parameters ++ " /usr/sbin/kakapo ,169.254.0.11,64504 ,169.254.0.12,64504"
        -- buildCommand parameters = "sudo " ++ parameters ++ " /usr/sbin/kakapo"

    -- mapM_ ( rsh . buildCommand ) ( blockGen [1..2] [1..2] base )
    mapM_ ( rsh . buildCommand ) ( blockGen [1..20] [1..500] base )
    --mapM_ ( rsh . buildCommand ) ( blockGen bsr gsr base )
    --mapM_ ( rsh . buildCommand ) ( blockGen bsrx gsr base )
    --mapM_ ( rsh . buildCommand ) ( blockGen bsr gsrx base )

