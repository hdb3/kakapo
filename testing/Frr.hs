module Main where
import System.Environment(getArgs)

main = do
    args <- getArgs
    let peerCount = if null args then 5 else read (head args) :: Int
    putStrLn $ unlines header
    putStrLn $ unlines $ peer monitor_as monitor_ip
    putStrLn $ unlines $ peers local_as start_ip peerCount

--peerCount = 20
local_ip = 13
monitor_ip = 19
monitor_as = "64505"
start_ip = 20
local_as = "64504"
base_address = "172.18.0."
addr n = base_address ++ show n

header = [ "! auto generated configuration file for FRR"
         , "password zebra"
         , "bgp as-path access-list al1 deny _1_2_"
         , "route-map rm1 permit 10"
         , "  match as-path al1"
         , "router bgp " ++ local_as
         , "bgp router-id " ++ addr local_ip
         ]

peerLine as ip s = unwords [ "neighbor ", addr ip, s ]
peer as ip = map (peerLine as ip) [ "remote-as " ++ as
                                  , "update-source " ++ addr local_ip
                                  , "solo"
                                  , "route-map rm1 in"
                                  ]

peers as start n = concatMap (peer as) [start .. start + n] 
