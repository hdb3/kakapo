module Main where
import System.Environment(getArgs)
import Data.List(intercalate)

{-
$KAKAPO 172.18.0.13,172.18.19,64505 \
 172.18.0.13,172.18.0.20,64504 172.18.0.13,172.18.0.21,64504 172.18.0.13,172.18.0.22,64504 172.18.0.13,172.18.0.23,64504 172.18.0.13,172.18.0.24,64504

 - -}
main = do
    args <- getArgs
    let peerCount = read (head args) :: Int
    putStr header
    putStrLn $ peers local_as local_ip start_ip peerCount


local_ip = 13
monitor_ip = 19
monitor_as = "64505"
start_ip = 20
local_as = "64504"
base_address = "172.18.0."
addr n = base_address ++ show n
uncommas = intercalate ","

--header =  "$KAKAPO 172.18.0.13,172.18.19,64505"
header =  "$KAKAPO " ++ addr local_ip ++ "," ++ addr monitor_ip ++ "," ++ monitor_as ++ " "


peer as local_ip remote_ip = uncommas [ addr local_ip , addr remote_ip , as ]

peers as local_ip start n = unwords $ map (peer as local_ip) [start .. start + n -1]
