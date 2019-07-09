module Main where
import System.Exit(die)
import Data.Maybe(fromJust,isJust)
import System.Environment(getArgs)
import Data.List(lookup,partition,isPrefixOf,elem)
import Data.Char(isControl)
import Control.Monad(when,unless,void)
import Data.UUID(toString)
import Data.UUID.V1(nextUUID)
import Data.Time.Clock.System ( systemSeconds, getSystemTime )

import Docker
import Presets

kakapoSender   = "172.18.0.21"
kakapoReceiver = "172.18.0.22"
sutAddress     = "172.18.0.13"
webDAVAddress  = "10.30.65.209"
kakapoBinary = "/usr/sbin/kakapo"
localAS = "64504"

getTopic s = fromJust $ lookup s presetTopics 

optSet s = ( elem ("--" ++ s) ) <$> getArgs

main = do
    args <- filter (not . isPrefixOf "--") <$> getArgs
    oldSchool <- optSet "old"
    uuid <- ( toString . fromJust ) <$> nextUUID
    time <- (show . systemSeconds ) <$> getSystemTime

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

    let (repo , app ) = (fromJust platform)
        sutHostName = (args !! 2)
        kakapoHostName = (args !! 3)

        sut = docker sutHostName
        kakapo = docker kakapoHostName

    let dockerFlags name = [ "-v", "/coredumps:/coredumps", "--privileged", "--rm" , "--network" , "host" , "--hostname" , name, "--name", name ]
        dockerInteractive name = "-i" : dockerFlags name
        dockerDaemon name = "-d" : dockerFlags name
        dockerRun host flags repo commands = docker host $ ["run"] ++ flags ++ [ repo ] ++ commands 
    
-- We need to run the app in  sysinfo mode before starting everything else,
-- in order to push the app verion string and sysinfo data into the kakapo environment
-- (from whence it will eventualy emerge in the logs)

    sut ["kill",app]
    sut ["pull",repo]

    sysinfo <- maybe "VERSION=\"UNKNOWN\" MEMSIZE=-1 CORES=-1 THREADS=-1" (map (\c -> if isControl c then ' ' else c)) <$>
                   dockerCMD sutHostName ( ["run"] ++ dockerInteractive app ++ [ repo , "sysinfo"] ) ""
    putStrLn $ "Sysinfo: " ++ sysinfo


    -- if ruuning the SUT app for multiple instances of kakapo then start it here...
    -- void $ dockerRun sutHostName (dockerDaemon app) repo []
    -- let kakapoDocker parameters = void $ dockerCMD kakapoHostName ( ["run"] ++ dockerInteractive "kakapo" ++ ["--entrypoint" , "/usr/bin/bash" , registry ++ "kakapo" ]) (unwords parameters)


    let kakapoDocker parameters = do sut ["kill",app]
                                     void $ dockerRun sutHostName (dockerDaemon app) repo []
                                     void $ dockerCMD kakapoHostName ( ["run"] ++ dockerInteractive "kakapo" ++ ["--entrypoint" , "/usr/bin/bash" , registry ++ "kakapo" ]) (unwords parameters)

    let
        logText = "TOPIC=\'" ++ topic ++ "\' " ++ " PLATFORM=" ++ app ++ " SUT=" ++ sutHostName ++ " " ++ " TIME=" ++ time ++ " UUID=" ++ uuid ++ " " ++ sysinfo
        base = kvSet "LOGPATH" ( webDAVAddress ++ "/" ++ app ) $ kvSet "LOGTEXT" ( "\"" ++ logText ++ "\"") kakapoDefaultParameters

        expandParameters = map (\(k,v) -> k ++ "=" ++ v)
        kakapoDefaultParameters = -- NOTE!!! must include all parameters required even if otherwise set
                                  -- kvSet cannot insert (obvs this is not elegant....)
           [
             ("LOGTEXT" , "\"LOGTEXT not provided\" "),
             ("LOGPATH" , webDAVAddress),
             ("SLEEP" , "0 "),
             ("MAXBURSTCOUNT" , "1 "),
             ("IDLETHR" , "3 "),
             ("GROUPSIZE" , "10 "),
             ("BLOCKSIZE" , "1 "),
             ("CYCLEDELAY" , "0 "),
             if oldSchool then 
                 ("FASTCYCLELIMIT" , "0 ")
             else
                 ("FASTCYCLELIMIT" , "1 "),
             ("CYCLECOUNT" , "10 "),
             ("NEXTHOP" , kakapoSender ++ " ")
           ]

        -- TODO - eliminate this function by using 'nubBy' over a list built by 'cons'ing the elemnts (nub will discard repeats)
        kvSet k v = map (\(a,b) -> if a == k then (a,v) else (a,b) )

        blockGen bsRange gsRange count burstRange = [ expandParameters $ kvSet "CYCLECOUNT" (show count)
                                                                       $ kvSet "GROUPSIZE" ( show gs )
                                                                       $ kvSet "BLOCKSIZE" ( show bs )
                                                                       $ kvSet "MAXBURSTCOUNT" (show burst)
                                                                       base | bs <- bsRange , gs <- gsRange, burst <- burstRange ]

        -- kakapo in passive mode:
        --buildCommand parameters = parameters ++ [ "/usr/sbin/kakapo" , ",172.18.0.11,64504" , ",172.18.0.12,64504" ]

        -- kakapo in active mode:
        buildCommand parameters = parameters ++ [ kakapoBinary
                                                , sutAddress ++ "," ++ kakapoSender ++ "," ++ localAS
                                                , sutAddress ++ "," ++ kakapoReceiver ++ "," ++ localAS ]

    let (b,g,c,burstRange) = getTopic topic

    kakapo ["kill","kakapo"]
    kakapo ["pull",registry ++ "kakapo"]
    kakapoDocker [ "ip" , "address" , "add" , kakapoSender   ++ "/32" , "dev" , "lo"]
    kakapoDocker [ "ip" , "address" , "add" , kakapoReceiver ++ "/32" , "dev" , "lo"]

    mapM_ ( kakapoDocker . buildCommand ) ( blockGen b g c burstRange )

    sut ["kill",app]

    putStrLn "Done"
