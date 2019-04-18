module Main where
import Data.List(intercalate)
import Runner

main = do
    let shell = bash
        sut = ssh [ "root@192.168.122.18" ]
        logtext = "\"BIRD nested batch run 1\""
        base = kvSet "LOGTEXT" logtext kakapoDefaultParameters
        --gs n = kvSet "GROUPSIZE" (show n) $ kvSet "LOGTEXT" logtext kakapoDefaultParameters

        gsr = [1..10]
        gsrx = [10,20..50]
        bsr = [1..10] ++ [10,20..100] ++ [100,200..1000] ++ [1000,2000..10000] ++ [10000,20000..100000]
        bsrx = [100000,200000..1000000]
        --testSet = kvGen "BLOCKSIZE" [1..10] ( kvSet "LOGTEXT" "\"BIRD batch run 2\"" kakapoDefaultParameters )
        --runScript = genCommands "sudo" testSet "/home/nic/src/kakapo/kakapo"
    --putStrLn $ unlines runScript
    sut "docker kill bird"
    sut "docker pull hdb3/bird ; docker run --name bird --privileged -d -v /var/www/thttpd/:/var/www/thttpd/ --network host --rm hdb3/bird"
    -- mapM_ shell runScript
    --mapM_ kakapo ( map expandParameters $ kvGen "BLOCKSIZE" [1..10] ( gs 1) )
    mapM_ kakapo $ blockGen bsr gsrx base
    mapM_ kakapo $ blockGen bsrx gsr base
    --mapM_ shell $ kvGen "BLOCKSIZE" [10..100] $ gs 1
    --mapM_ shell $ kvGen "BLOCKSIZE" [100..1000] $ gs 1
    sut "docker kill bird"
    putStrLn "Done"

shell = bash
kakapo parameters = shell $ "sudo " ++ parameters ++ " /home/nic/src/kakapo/kakapo" 
genCommands :: String -> [[(String,String)]] -> String -> [String]
genCommands pre parameterLists post = map (\parameterList -> intercalate " " [pre, expandParameters parameterList, post ]) parameterLists
expandParameters = intercalate " " . map (\(k,v) -> k ++ "=" ++ v)
kakapoDefaultParameters =
   [
     ("LOGTEXT" , "\"auto BIRD\" "),
     ("MYAS" , "64504 "),
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
