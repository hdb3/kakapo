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

getTopic s = fromJust $ lookup s presetTopics 

optSet s = ( elem ("--" ++ s) ) <$> getArgs
--oldSchool = ( elem "--old") <$> getArgs

main = do
    args <- filter (not . isPrefixOf "--") <$> getArgs

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
    
    sut ["kill",app]
    sut ["pull",repo]

    sysinfo <- maybe "VERSION=\"UNKNOWN\" MEMSIZE=-1 CORES=-1 THREADS=-1" (map (\c -> if isControl c then ' ' else c)) <$>
                   dockerCMD sutHostName ( ["run"] ++ dockerInteractive app ++ [ repo , "sysinfo"] ) ""
    putStrLn $ "Sysinfo: " ++ sysinfo

    -- if ruuning the SUT app for muktiple instances of kakapo then start it here...
    -- void $ dockerRun sutHostName (dockerDaemon app) repo []
    -- let kakapoDocker parameters = void $ dockerCMD kakapoHostName ( ["run"] ++ dockerInteractive "kakapo" ++ ["--entrypoint" , "/usr/bin/bash" , registry ++ "kakapo" ]) (unwords parameters)

    kakapo ["kill","kakapo"]
    kakapo ["pull",registry ++ "kakapo"]

    let kakapoDocker parameters = do sut ["kill",app]
                                     void $ dockerRun sutHostName (dockerDaemon app) repo []
                                     void $ dockerCMD kakapoHostName ( ["run"] ++ dockerInteractive "kakapo" ++ ["--entrypoint" , "/usr/bin/bash" , registry ++ "kakapo" ]) (unwords parameters)

    kakapoDocker [ "ip" , "address" , "add" , "172.18.0.21/32" , "dev" , "lo"]
    kakapoDocker [ "ip" , "address" , "add" , "172.18.0.22/32" , "dev" , "lo"]

    uuid <- ( toString . fromJust ) <$> nextUUID
    time <- (show . systemSeconds ) <$> getSystemTime
    oldSchool <- optSet "old"
    let
        logText = "TOPIC=\'" ++ topic ++ "\' " ++ " PLATFORM=" ++ app ++ " SUT=" ++ sutHostName ++ " " ++ " TIME=" ++ time ++ " UUID=" ++ uuid ++ " " ++ sysinfo
        base = kvSet "LOGPATH" ( "10.30.65.209/" ++ app ) $ kvSet "LOGTEXT" ( "\"" ++ logText ++ "\"") kakapoDefaultParameters

        expandParameters = map (\(k,v) -> k ++ "=" ++ v)
        kakapoDefaultParameters = -- NOTE!!! must include all parameters required even if otherwise set
                                  -- kvSet cannot insert (obvs this is not elegant....)
           [
             ("LOGTEXT" , "\"LOGTEXT not provided\" "),
             ("LOGPATH" , "10.30.65.209"),
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
             ("NEXTHOP" , "172.18.0.21 ")
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
        buildCommand parameters = parameters ++ [ "/usr/sbin/kakapo" , "172.18.0.13,172.18.0.21,64504" , "172.18.0.13,172.18.0.22,64504" ]

    let (b,g,c,burstRange) = getTopic topic

    mapM_ ( kakapoDocker . buildCommand ) ( blockGen b g c burstRange )

    sut ["kill",app]

    putStrLn "Done"
