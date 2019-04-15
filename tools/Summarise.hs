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
   let rtt = map readFloat $ tail $ lookup__ "RTT" dATA -- intentionally discarding the first value as this is always very different, and larger, than the rest
       rttCount = fromIntegral $ length rtt
       rttSum = sum rtt
       rttMean = rttSum / rttCount
       rttSqSum = foldl (\x y -> x + y * y) 0 rtt
       rttSD    = ( sqrt ( (rttCount * rttSqSum) - (rttSum * rttSum) )) / rttCount
       rttRSD   = rttSD / rttMean 

   putStrLn $ "(rttMean / rttSD / rttRSD) = " ++ show (rttMean, rttSD, rttRSD)
