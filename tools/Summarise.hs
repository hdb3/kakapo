module Main where
import Data.List(intercalate)
import Data.Maybe(fromMaybe,fromJust)
import Sections hiding (main)

main = do
   infile <- getContents

   let (header,dATA,_) = getSection infile
   putStrLn $ "header fields found: " ++ intercalate ", " (map fst header) -- concat ( intersperse ", " (map fst header))
   let lookup_ k d = fromMaybe "<not found>" (lookup k d)
   let lookup__ k d = fromJust (lookup k d)
   let readFloat s = read s :: Double
   putStrLn $ "dataset \"" ++ lookup_ "DESC" header ++ "\" timed at " ++ lookup_ "START" header
   let metrics = map fst dATA
   putStrLn $ "metrics found: " ++ intercalate ", " metrics -- concat ( intersperse ", " metrics )
   putStrLn $ "sample size: " ++ ( show . length . snd . head ) dATA
   let metric tag = map readFloat $ tail $ lookup__ tag dATA -- intentionally discarding the first value as this is always very different, and larger, than the rest
   let rtt = metric "RTT"

   putStrLn $ "rtt: (count / mean / sd / rsd) = " ++ show ( getMeanSDandRSD ( metric "RTT"))
   putStrLn $ "latency: (count / mean / sd / rsd) = " ++ show ( getMeanSDandRSD ( metric "LATENCY"))

getMeanSDandRSD :: [Double] -> (Double,Double,Double,Double)
getMeanSDandRSD sample = 
    let
       count = fromIntegral $ length sample
       ssum = sum sample
       mean = ssum / count
       sqSum = foldl (\x y -> x + y * y) 0 sample
       sd    = ( sqrt ( (count * sqSum) - (ssum * ssum) )) / count
       rsd   = sd / mean 
    in (count, mean, sd, rsd)
