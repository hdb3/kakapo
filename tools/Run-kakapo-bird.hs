module Main where
import Data.List(intercalate)
import Runner
import System.Exit
import Data.Char(isControl)

main = do
    let
        sutPath = [ "root@docker02" ]
        sut = ssh sutPath
        getSUT = getSSH sutPath
        kakapo = ssh [ "-p" , "65535" , "root@10.30.65.214" ]
        kakapoHost = ssh [ "root@10.30.65.214" ]

        -- sut = ssh [ "root@localhost" ]
        -- kakapo = ssh [ "-p" , "65535" , "root@localhost" ]
        -- kakapoHost = sut

        repo = "hdb3/bird"
        app  = "bird"

    sut $ "docker kill " ++ app
    sut $ "docker pull " ++ repo
    sysinfo' <- getSUT $ "docker run --name " ++ app ++ " --privileged -t --rm " ++ repo ++ " sysinfo"
    let sysinfo = maybe "VERSION=\"UNKNOWN\" MEMSIZE=-1 CORES=-1 THREADS=-1" (map (\c -> if isControl c then ' ' else c)) sysinfo'
    putStrLn $ "Sysinfo: " ++ sysinfo

    sut $ "docker run --name " ++ app ++ " --privileged -d --network host --rm " ++ repo

    kakapoHost "docker kill kakapo"
    kakapoHost "docker kill kakapo ; docker pull hdb3/kakapo ; docker run --name kakapo --privileged --rm -d --network host --hostname kakapo hdb3/kakapo"

    runExperiment kakapo sysinfo

    sut $ "docker kill " ++ app
    kakapoHost "docker kill kakapo"

    putStrLn "Done"

runExperiment :: ([Char] -> IO()) -> String -> IO()
runExperiment rsh logtext = do
    let
        base = kvSet "LOGPATH" "10.30.65.209/bird" $ kvSet "LOGTEXT" ( "\"" ++ logtext ++ "\"") kakapoDefaultParameters
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
             ("CYCLECOUNT" , "20 "),
             ("NEXTHOP" , "169.254.0.11 ")
           ]

        kvSet k v = map (\(a,b) -> if a == k then (a,v) else (a,b) )

        kvGen k vx m = map (\v -> kvSet k ( show v) m) vx

        blockGen bsRange gsRange base = [ expandParameters $ kvSet "GROUPSIZE" ( show gs ) $ kvSet "BLOCKSIZE" ( show bs ) base | bs <- bsRange , gs <- gsRange ]

        buildCommand parameters = parameters ++ " /usr/sbin/kakapo ,169.254.0.11,64504 ,169.254.0.12,64504"
        -- buildCommand parameters = "sudo " ++ parameters ++ " /usr/sbin/kakapo"

    -- mapM_ ( rsh . buildCommand ) ( blockGen [1..2] [1..2] base )
    mapM_ ( rsh . buildCommand ) ( blockGen bsr gsr base )
    mapM_ ( rsh . buildCommand ) ( blockGen bsrx gsr base )
    mapM_ ( rsh . buildCommand ) ( blockGen bsr gsrx base )
