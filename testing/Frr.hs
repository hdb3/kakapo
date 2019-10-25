{-#  LANGUAGE QuasiQuotes  #-}
module Main where
import System.Environment(getArgs)
import Text.RawString.QQ -- raw-strings-qq

main = do
    args <- getArgs
    let peerCount = if null args then 5 else read (head args) :: Int
        filterFlag = 2 > length args
    putStrLns header
    if filterFlag then putStrLn routeMap2 else return ()
    -- if filterFlag then putStrLns routeMap1 else return ()
    putStrLns sectionBGP
    putStrLns $ peer False monitor_as monitor_ip
    putStrLns $ peers filterFlag local_as start_ip peerCount
    putStrLns trailer

putStrLns s = putStrLn $ unlines s

local_ip = 13
monitor_ip = 19
monitor_as = "64505"
start_ip = 20
local_as = "64504"
base_address = "172.18.0."
addr n = base_address ++ show n

header = [ "! auto generated configuration file for FRR"
         , "password zebra"
         ]

routeMap1 = [ "bgp as-path access-list al1 deny _1_2_"
            , "route-map rm1 permit 10"
            , "  match as-path al1"
            ]

routeMap2 = [r|
bgp as-path access-list al1 permit ^99_98_97_96$
bgp as-path access-list al1 permit _99_98_97_96_
bgp as-path access-list al1 permit _99_98_([0..9]*)_97_96_
bgp as-path access-list al2 permit _([0..9]*)_
bgp as-path access-list al3 permit _([0..9]*)_
route-map rm1 deny 10
  match as-path al1
route-map rm1 permit 20
  match as-path al2
route-map rm1 deny 30
  match as-path al3
route-map rm1 permit 99|]

sectionBGP = [ "router bgp " ++ local_as
             , "bgp router-id " ++ addr local_ip
             ]

trailer = ["line vty"
          , " exec-timeout 0 0"
          , " no login"
          ]

peerLine as ip s = unwords [ " neighbor ", addr ip, s ]
peer rm as ip = "" : ( map (peerLine as ip) ( [ " remote-as " ++ as
                                            , " update-source " ++ addr local_ip
                                            , " solo"
                                            ] ++ [" route-map rm1 in" | rm]))
                                            -- ( if rm then [" route-map rm1 in" ] else []))


peers rm as start n = concatMap (peer rm as) [start .. start + n -1] 
