module Mean where
import Data.List(sort)

mean :: [Double] -> Double
mean = (\(_,x,_,_) -> x) . meanRSD

meanRSD :: [Double] -> (Int,Double,Double,Double)
meanRSD sample = 
        let
           count = fromIntegral $ length sample
           ssum  = sum sample
           mean'  = ssum / count
           sqSum = foldl (\x y -> x + y * y) 0 sample
           sd    = sqrt ( (count * sqSum) - (ssum * ssum) ) / count
           rsd   = sd / mean' 
        in (length sample, mean', sd, rsd)

meanSD :: [Double] -> (Double,Double)
meanSD = (\(_,m,sd,_) -> (m,sd)) . meanRSD

sndLeast :: [Double] -> Double
sndLeast sample | 1 < length sample = sort sample !! 1
                | otherwise =  minimum sample


maxMinMean  :: [Double] -> (Double,Double,Double)
maxMinMean sample = ( mean sample , minimum sample , maximum sample )

maxSDMinMean  :: [Double] -> (Double,Double,Double,Double)
maxSDMinMean sample = let (_,mn,sd,_) = meanRSD sample in ( mn, sd, minimum sample , maximum sample )

minSD :: [Double] -> (Double,Double)
minSD [] = (0,0)
minSD sample =  (minimum sample , mean sample)
