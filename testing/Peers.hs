module Main where
import System.Environment(getArgs)

{-
$KAKAPO 172.18.0.13,172.18.19,64505 \
 172.18.0.13,172.18.0.20,64504 172.18.0.13,172.18.0.21,64504 172.18.0.13,172.18.0.22,64504 172.18.0.13,172.18.0.23,64504 172.18.0.13,172.18.0.24,64504

 - -}
main = do
    args <- getArgs
    let peerCount = read (head args) :: Int
    putStrLns header
    putStrLns $ peers filterFlag local_as start_ip peerCount

putStrLns s = putStrLn $ unlines s

local_ip = 13
monitor_ip = 19
monitor_as = "64505"
start_ip = 20
local_as = "64504"
base_address = "172.18.0."
addr n = base_address ++ show n

--header =  "$KAKAPO 172.18.0.13,172.18.19,64505"
header =  "$KAKAPO " ++ addr local_ip ++ "," ++ addr monitor_ip ++ "," ++ monitor_as
         


peerLine as ip s = unwords [ " neighbor ", addr ip, s ]
peer rm as ip = "" : ( map (peerLine as ip) ( [ " remote-as " ++ as
                                            , " update-source " ++ addr local_ip
                                            , " solo"
                                            ] ++ [" route-map rm1 in" | rm]))
                                            -- ( if rm then [" route-map rm1 in" ] else []))


peers rm as start n = concatMap (peer rm as) [start .. start + n -1] 
