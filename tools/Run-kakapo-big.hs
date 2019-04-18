module Main where
import Data.List(intercalate)
import Runner

main = do
    let 
        sut = ssh [ "root@docker02" ]
        kakapo = ssh [ "-p" , "65535" , "root@10.30.65.214" ]
        kakapoHost = ssh [ "root@10.30.65.214" ]
        runKakapo parameters = kakapo $ "sudo " ++ parameters ++ " /usr/sbin/kakapo" 

        logtext = "\"FRR nested batch run 1\""
        base = kvSet "LOGTEXT" logtext kakapoDefaultParameters
        gsr = [1..10]
        gsrx = [10,20..50]
        bsr = [1..10] ++ [10,20..100] ++ [100,200..1000] ++ [1000,2000..10000] ++ [10000,20000..100000]
        bsrx = [100000,200000..1000000]

    sut "docker kill frr"
    sut "docker pull hdb3/frr ; docker run --name frr --privileged -d -v /var/www/thttpd/:/var/www/thttpd/ --network host --rm hdb3/frr"

    kakapoHost "docker kill kakapo"
    kakapoHost "docker pull hdb3/kakapo ; docker run --name kakapo --privileged --rm -d --network host --hostname kakapo hdb3/kakapo"

    --mapM_ runKakapo $ blockGen bsr gsrx base
    --mapM_ runKakapo $ blockGen bsrx gsr base
    runKakapo $ expandParameters base

    sut "docker kill frr"
    kakapoHost "docker kill kakapo"

    putStrLn "Done"

genCommands :: String -> [[(String,String)]] -> String -> [String]
genCommands pre parameterLists post = map (\parameterList -> intercalate " " [pre, expandParameters parameterList, post ]) parameterLists
expandParameters = intercalate " " . map (\(k,v) -> k ++ "=" ++ v)
kakapoDefaultParameters =
   [
     ("LOGTEXT" , "\"auto FRR\" "),
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
