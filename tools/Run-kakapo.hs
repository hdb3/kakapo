module Main where
import System.Exit(die)
import Data.Maybe(fromJust,isJust)
import System.Environment(getArgs)
import Data.List(lookup)
import Data.Char(isControl)
import Control.Monad(when,unless,void)
import Data.UUID(toString)
import Data.UUID.V1(nextUUID)
import Data.Time.Clock.System ( systemSeconds, getSystemTime )

import Docker

frr = ( "hdb3/frr", "frr")
bird = ("hdb3/bird","bird")
hbgp = ("hdb3/hbgp","hbgp")
relay = ("hdb3/relay","relay")

presetPlatforms = [("bird",bird),("frr",frr),("hbgp",hbgp),("relay",relay)]

presetTopics = [
                 ("SMOKETEST", ( [1..2] , [1..2] , 2))
               , ("BASIC", ( [1..10] ++ [20,30..100] ++[200,300..1000] ++[2000,3000..10000] ++[20000,30000..100000] , [1,2,5,10] , 10))
               , ("EXPBS", ( expPlimited 10 1000000 , [1] , 20))
               , ("EXPBSLARGE", ( expPlimited 10 100000 , [10] , 20))
               , ("EXPBSVLARGE", ( expPlimited 10 10000 , [100] , 20))
               ]

expPlimited pow limit = takeWhile ( limit+1 > ) $ map floor $ go 1.0
    where go n = n : go (n * (10 ** (1/pow)))

getTopic s = fromJust $ lookup s presetTopics 

main = do
    args <- getArgs

    when (null args)
         (die $ "please specify topic, platform, SUT target and kakapo target\n" ++ show presetTopics ++ show presetPlatforms)

    unless (4 == length args)
         (die "expecting exactly 4 parameters (topic, platform, SUT target and kakapo target)")

    let topic = args !! 0
    unless (isJust $ lookup topic presetTopics)
         (die $ "unknown topic\n" ++ show presetTopics)

    let platform = lookup ( args !! 1 ) presetPlatforms
    unless (isJust platform)
         (die $ "unknown platform\n" ++ show presetPlatforms)

    start topic (fromJust platform) (args !! 2) (args !! 3)

start :: String -> (String , String ) -> String -> String -> IO ()
start topic (repo , app ) sutHostName  kakapoHostName = do
    let
        sut = docker sutHostName
        kakapo = docker kakapoHostName

    let dockerFlags name = [ "--privileged", "--rm" , "--network" , "host" , "--hostname" , name, "--name", name ]
        dockerInteractive name = "-i" : dockerFlags name
        dockerDaemon name = "-d" : dockerFlags name
        dockerRun host flags repo commands = docker host $ ["run"] ++ flags ++ [ repo ] ++ commands 
    
    sut ["kill",app]
    
    sut ["pull",repo]
    sysinfo' <- dockerCMD sutHostName ( ["run"] ++ dockerInteractive app ++ [ repo , "sysinfo"] ) ""
    let sysinfo = maybe "VERSION=\"UNKNOWN\" MEMSIZE=-1 CORES=-1 THREADS=-1" (map (\c -> if isControl c then ' ' else c)) sysinfo'
    putStrLn $ "Sysinfo: " ++ sysinfo

    
    void $ dockerRun sutHostName (dockerDaemon app) repo []

    kakapo ["kill","kakapo"]
    kakapo ["pull","hdb3/kakapo"]

    -- let kakapoDocker = dockerRun kakapoHostName (dockerInteractive "kakapo" ++ ["--entrypoint" , "/usr/sbin/kakapo"]) "hdb3/kakapo" 
    let kakapoDocker parameters = void $ dockerCMD kakapoHostName ( ["run"] ++ dockerInteractive "kakapo" ++ ["--entrypoint" , "/usr/bin/bash" , "hdb3/kakapo" ]) (unwords parameters)

    runExperiment kakapoDocker sutHostName topic sysinfo app

    putStrLn "Done"

runExperiment :: ([String] -> IO()) -> String -> String -> String -> String -> IO()
runExperiment rsh sut topic sysinfo app = do
    uuid <- ( toString . fromJust ) <$> nextUUID
    time <- (show . systemSeconds ) <$> getSystemTime
    let
        logText = "TOPIC=\'" ++ topic ++ "\' " ++ " SUT=" ++ sut ++ " " ++ " TIME=" ++ time ++ " UUID=" ++ uuid ++ " " ++ sysinfo
        base = kvSet "LOGPATH" ( "10.30.65.209/" ++ app ) $ kvSet "LOGTEXT" ( "\"" ++ logText ++ "\"") kakapoDefaultParameters
        gsr = [1..10]
        gsrx = [10,20..50]
        bsr = [1..10] ++ [10,20..100] ++ [100,200..1000] ++ [1000,2000..10000] ++ [10000,20000..100000]
        bsr0 = [1..10]
        bsrx = [100000,200000..1000000]

        genCommands pre parameterLists post = map (\parameterList -> [pre, expandParameters parameterList, post ]) parameterLists
        expandParameters = map (\(k,v) -> k ++ "=" ++ v)
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

        --buildCommand parameters = parameters ++ [ ",169.254.0.11,64504" , ",169.254.0.12,64504" ]
        buildCommand parameters = parameters ++ [ "/usr/sbin/kakapo" , ",169.254.0.11,64504" , ",169.254.0.12,64504" ]

    let (b,g,c) = getTopic topic
    mapM_ ( rsh . buildCommand ) ( blockGen b g c base )
    return ()
