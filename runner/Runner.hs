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
import Presets

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
start topic (repo , app ) sutHostName kakapoHostName = do
    let
        sut = docker sutHostName
        kakapo = docker kakapoHostName

    let dockerFlags name = [ "-v", "coredumps:/cores", "--privileged", "--rm" , "--network" , "host" , "--hostname" , name, "--name", name ]
        dockerInteractive name = "-i" : dockerFlags name
        dockerDaemon name = "-d" : dockerFlags name
        dockerRun host flags repo commands = docker host $ ["run"] ++ flags ++ [ repo ] ++ commands 
    
    sut ["kill",app]
    
    sut ["pull",repo]
    sysinfo <- maybe "VERSION=\"UNKNOWN\" MEMSIZE=-1 CORES=-1 THREADS=-1" (map (\c -> if isControl c then ' ' else c)) <$>
                   dockerCMD sutHostName ( ["run"] ++ dockerInteractive app ++ [ repo , "sysinfo"] ) ""
    putStrLn $ "Sysinfo: " ++ sysinfo

    
    void $ dockerRun sutHostName (dockerDaemon app) repo []

    kakapo ["kill","kakapo"]
    kakapo ["pull",registry ++ "kakapo"]

    let kakapoDocker parameters = void $ dockerCMD kakapoHostName ( ["run"] ++ dockerInteractive "kakapo" ++ ["--entrypoint" , "/usr/bin/bash" , registry ++ "kakapo" ]) (unwords parameters)

    kakapoDocker [ "ip" , "address" , "add" , "172.18.0.21/32" , "dev" , "lo"]
    kakapoDocker [ "ip" , "address" , "add" , "172.18.0.22/32" , "dev" , "lo"]
    runExperiment kakapoDocker sutHostName topic sysinfo app
    sut ["kill",app]

    putStrLn "Done"

runExperiment :: ([String] -> IO()) -> String -> String -> String -> String -> IO()
runExperiment rsh sut topic sysinfo app = do
    uuid <- ( toString . fromJust ) <$> nextUUID
    time <- (show . systemSeconds ) <$> getSystemTime
    let
        logText = "TOPIC=\'" ++ topic ++ "\' " ++ " PLATFORM=" ++ app ++ " SUT=" ++ sut ++ " " ++ " TIME=" ++ time ++ " UUID=" ++ uuid ++ " " ++ sysinfo
        base = kvSet "LOGPATH" ( "10.30.65.209/" ++ app ) $ kvSet "LOGTEXT" ( "\"" ++ logText ++ "\"") kakapoDefaultParameters

        genCommands pre parameterLists post = map (\parameterList -> [pre, expandParameters parameterList, post ]) parameterLists
        expandParameters = map (\(k,v) -> k ++ "=" ++ v)
        kakapoDefaultParameters =
           [
             ("LOGTEXT" , "\"LOGTEXT not provided\" "),
             ("LOGPATH" , "10.30.65.209"),
             ("SLEEP" , "0 "),
             ("MAXBURSTCOUNT" , "1 "),
             ("GROUPSIZE" , "10 "),
             ("BLOCKSIZE" , "1 "),
             ("CYCLEDELAY" , "0 "),
             ("FASTCYCLELIMIT" , "0 "),
             ("CYCLECOUNT" , "10 "),
             ("NEXTHOP" , "172.18.0.21 ")
           ]

        kvSet k v = map (\(a,b) -> if a == k then (a,v) else (a,b) )

        kvGen k vx m = map (\v -> kvSet k ( show v) m) vx

        blockGen bsRange gsRange count base = [ expandParameters $ kvSet "CYCLECOUNT" (show count) $ kvSet "GROUPSIZE" ( show gs ) $ kvSet "BLOCKSIZE" ( show bs ) base | bs <- bsRange , gs <- gsRange ]

        -- kakapo in passive mode:
        --buildCommand parameters = parameters ++ [ "/usr/sbin/kakapo" , ",172.18.0.11,64504" , ",172.18.0.12,64504" ]

        -- kakapo in active mode:
        buildCommand parameters = parameters ++ [ "/usr/sbin/kakapo" , "172.18.0.13,172.18.0.21,64504" , "172.18.0.13,172.18.0.22,64504" ]

    let (b,g,c) = getTopic topic
    mapM_ ( rsh . buildCommand ) ( blockGen b g c base )
    return ()
