module Main where
import System.IO(stderr,hPutStrLn,hPrint)
import System.Exit(die)
import Data.Maybe(fromJust,isJust)
import System.Environment(getArgs)
import Data.List(lookup)
import Data.Char(isControl)
import Control.Monad(when,unless)
import Data.UUID(toString)
import Data.UUID.V1(nextUUID)

import Runner

localhost = ( "root@localhost" , "root@localhost" )
docker02 = ( "root@docker02" , "root@10.30.65.214" )

frr = ( "hdb3/frr", "frr")
bird = ("hdb3/bird","bird")

presetTargets = [("localhost",localhost) , ("docker02",docker02) ]
presetPlatforms = [("bird",bird),("frr",frr)]

presetTopics = [
                 ("SMOKETEST", ( [1..2] , [1..2] , 2))
               , ("BASIC", ( [1..10] ++ [20,30..100] ++[200,300..1000] ++[2000,3000..10000] ++[20000,30000..100000] , [1,2,5,10] , 10))
               , ("EXPBS", ( expPlimited 5 1000000 , [1] , 20))
               , ("EXPBSLARGE", ( expPlimited 5 100000 , [10] , 20))
               , ("EXPBSVLARGE", ( expPlimited 5 10000 , [100] , 20))
               ]

expPlimited pow limit = takeWhile ( limit+1 > ) $ map floor $ go 1.0
    where go n = n : go (n * (10 ** (1/pow)))

getTopic s = fromJust $ lookup s presetTopics 

main = do
    args <- getArgs

    when (null args)
         (die $ "please specify topic, target systems and platforms\n" ++ showPresets)

    let topic = args !! 0         
    unless (isJust (lookup topic presetTopics))
         (die $ "unknown topic\n" ++ showPresets)

    if 5 == length args then do
        hPutStrLn stderr "using raw arguments from command line"
        start topic (args !! 1 , args !! 2 ) (args !! 3 , args !! 4 ) 
    else if 3 == length args then do
        let target = lookup ( args !! 1 ) presetTargets
            platform = lookup ( args !! 2 ) presetPlatforms
        if isJust platform && isJust target then do
            hPutStrLn stderr "using predefined arguments based on command line labels"
            start topic (fromJust target) (fromJust platform)
        else
            hPutStrLn stderr $ "could not find presets" ++ showPresets
    else
        hPutStrLn stderr $ "please specify topic, target systems and platforms\n" ++ showPresets
    where
        showPresets = unlines [ show ("Topics", presetTopics)
                              , show ("Targets", presetTargets)
                              , show ("Platforms", presetPlatforms) ]

start :: String -> (String , String ) -> ( String , String ) -> IO ()
start topic ( sutHostName , kakapoHostName) (repo , app ) = do
    let
        sut = sshLog "sut" [ sutHostName ]
        getSUT = getSSH [ sutHostName ]

        kakapo = sshLog "kakapo" [ "-p" , "65535" , kakapoHostName ]

        kakapoHost = sshLog "kakapoHost" [ kakapoHostName ]

    sut $ "docker kill " ++ app
    sut $ "docker pull " ++ repo
    sysinfo' <- getSUT $ "docker run --name " ++ app ++ " --privileged -t --rm " ++ repo ++ " sysinfo"
    let sysinfo = maybe "VERSION=\"UNKNOWN\" MEMSIZE=-1 CORES=-1 THREADS=-1" (map (\c -> if isControl c then ' ' else c)) sysinfo'
    putStrLn $ "Sysinfo: " ++ sysinfo

    sut $ "docker run --name " ++ app ++ " --privileged -d --network host --rm " ++ repo

    kakapoHost "docker kill kakapo"
    kakapoHost "docker kill kakapo ; docker pull hdb3/kakapo ; docker run --name kakapo --privileged --rm -d --network host --hostname kakapo hdb3/kakapo"

    runExperiment kakapo sutHostName topic sysinfo app

    sut $ "docker kill " ++ app
    kakapoHost "docker kill kakapo"

    putStrLn "Done"

runExperiment :: (String -> IO()) -> String -> String -> String -> String -> IO()
runExperiment rsh sut topic sysinfo app = do
    uuid <- ( toString . fromJust ) <$> nextUUID
    let
        logText = "TOPIC=\'" ++ topic ++ "\' " ++ " SUT=" ++ sut ++ " " ++ " UUID=" ++ uuid ++ " " ++ sysinfo
        base = kvSet "LOGPATH" ( "10.30.65.209/" ++ app ) $ kvSet "LOGTEXT" ( "\"" ++ logText ++ "\"") kakapoDefaultParameters
        gsr = [1..10]
        gsrx = [10,20..50]
        bsr = [1..10] ++ [10,20..100] ++ [100,200..1000] ++ [1000,2000..10000] ++ [10000,20000..100000]
        bsr0 = [1..10]
        bsrx = [100000,200000..1000000]

        genCommands :: String -> [[(String,String)]] -> String -> [String]
        genCommands pre parameterLists post = map (\parameterList -> unwords [pre, expandParameters parameterList, post ]) parameterLists
        expandParameters = unwords . map (\(k,v) -> k ++ "=" ++ v)
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

        blockGen bsRange gsRange count base = [ expandParameters $ kvSet "CYCLECOUNT" (show count) $ kvSet "GROUPSIZE" ( show gs ) $ kvSet "BLOCKSIZE" ( show bs ) base | bs <- bsRange , gs <- gsRange ]

        buildCommand parameters = parameters ++ " /usr/sbin/kakapo ,169.254.0.11,64504 ,169.254.0.12,64504"
        -- buildCommand parameters = "sudo " ++ parameters ++ " /usr/sbin/kakapo"

    -- mapM_ ( rsh . buildCommand ) ( blockGen [1..2] [1..2] base )
    let (b,g,c) = getTopic topic
    mapM_ ( rsh . buildCommand ) ( blockGen b g c base )
    -- mapM_ ( rsh . buildCommand ) ( blockGen [1..20] [1..500] 10 base )
    --mapM_ ( rsh . buildCommand ) ( blockGen bsr gsr base )
    --mapM_ ( rsh . buildCommand ) ( blockGen bsrx gsr base )
    --mapM_ ( rsh . buildCommand ) ( blockGen bsr gsrx base )

