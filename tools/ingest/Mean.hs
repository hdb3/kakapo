module Mean where
import qualified Data.Text as T
import Data.List(sort)

type Point = (Int,Double,Double,Double)
mean (_,x,_,_) = x
meanRSD (_,x,_,y) = (x,y)

readFloat :: T.Text -> Double
readFloat s = read (T.unpack s) :: Double

average ::  [ T.Text ] -> Point
average = getMeanSDandRSD . map readFloat
    where
    getMeanSDandRSD :: [Double] -> Point
    getMeanSDandRSD sample = 
        let
           count = fromIntegral $ length sample
           ssum  = sum sample
           mean  = ssum / count
           sqSum = foldl (\x y -> x + y * y) 0 sample
           sd    = sqrt ( (count * sqSum) - (ssum * ssum) ) / count
           rsd   = sd / mean 
        in (length sample, mean, sd, rsd)

least ::  [ T.Text ] -> Point
least = getLeast . map readFloat
    where
        getLeast :: [Double] -> Point
        getLeast sample = (length sample, minimum sample, 0.0, 0.0)

sndLeast ::  [ T.Text ] -> Point
sndLeast = getSndLeast . map readFloat
    where
        getSndLeast :: [Double] -> Point
        getSndLeast sample | 1 < length sample = (length sample, (sort sample) !! 1, 0.0, 0.0)
                           | otherwise = (length sample, minimum sample, 0.0, 0.0)
