module Mean where
import qualified Data.Text as T

type Point = (Int,Double,Double,Double)
mean (_,x,_,_) = x
meanRSD (_,x,_,y) = (x,y)

average ::  [ T.Text ] -> Point
average = getMeanSDandRSD . map readFloat
    where readFloat s = read (T.unpack s) :: Double

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
