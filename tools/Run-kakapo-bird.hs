module Main where
import Data.List(intercalate)
import Runner
import System.Exit

main = do
    let
        sut = ssh [ "root@docker02" ]
        kakapo = ssh [ "-p" , "65535" , "root@10.30.65.214" ]
        kakapoHost = ssh [ "root@10.30.65.214" ]

        -- sut = ssh [ "root@localhost" ]
        -- kakapo = ssh [ "-p" , "65535" , "root@localhost" ]
        -- kakapoHost = sut

    sut "docker kill bird"
    sut "docker pull hdb3/bird ; docker run --name bird --privileged -d -v /var/www/thttpd/:/var/www/thttpd/ --network host --rm hdb3/bird"

    kakapoHost "docker kill kakapo"
    kakapoHost "docker pull hdb3/kakapo ; docker run --name kakapo --privileged --rm -d --network host --hostname kakapo hdb3/kakapo"

    runExperiment kakapo

    sut "docker kill bird"
    kakapoHost "docker kill kakapo"

    putStrLn "Done"

runExperiment :: ([Char] -> IO()) -> IO()
runExperiment rsh = do
    let
        logtext = "\"BIRD nested batch run 1\""
        base = kvSet "LOGTEXT" logtext kakapoDefaultParameters
        gsr = [1..10]
        gsrx = [10,20..50]
        bsr = [1..10] ++ [10,20..100] ++ [100,200..1000] ++ [1000,2000..10000] ++ [10000,20000..100000]
        bsrx = [100000,200000..1000000]

        genCommands :: String -> [[(String,String)]] -> String -> [String]
        genCommands pre parameterLists post = map (\parameterList -> intercalate " " [pre, expandParameters parameterList, post ]) parameterLists
        expandParameters = intercalate " " . map (\(k,v) -> k ++ "=" ++ v)
        kakapoDefaultParameters =
           [
             ("LOGTEXT" , "\"auto BIRD\" "),
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

    --die $ buildCommand $ expandParameters base
    -- rsh $ buildCommand $ expandParameters base
    --mapM_ ( rsh . buildCommand ) ( blockGen bsr gsr base )
    mapM_ ( rsh . buildCommand ) ( blockGen bsrx gsr base )
    mapM_ ( rsh . buildCommand ) ( blockGen bsr gsrx base )
