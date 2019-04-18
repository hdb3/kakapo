module Main where

import Runner

main = do
    sysinfo <- bash "curl http://192.168.122.18/sysinfo.txt"
    version <- bash "curl http://192.168.122.18/version.txt"
    print (sysinfo,version)
