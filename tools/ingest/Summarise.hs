{-# LANGUAGE OverloadedStrings #-}
module Summarise where
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Sections hiding (main)

type Point = (Int,Double,Double,Double)
add :: Point -> Point -> Point
-- carefull, these are means!!!
add ( c1 , m1 , sd1 , rsd1 ) ( c2 , m2 , sd2 , rsd2 ) = let c12 = c1 + c2
                                                            fc1 = fromIntegral c1
                                                            fc2 = fromIntegral c2
                                                            fc12 = fromIntegral c12
                                                            m12 = ( m1 * fc1 + m2 * fc2 ) / fc12
                                                            sd12 = sqrt (( sd1^2 * fc1 + sd2^2 * fc2) / fc12)
                                                            rsd12 = sd12 / fc12
                                                        in ( c12 , m12 , sd12 ,rsd12 )
 
type DataPoint = ( [(T.Text , T.Text) ] , [( T.Text , Point) ])

main :: IO ()
main = processSection . fst . getSection . T.lines <$> T.getContents >>= print

processSection :: Section -> DataPoint
processSection (header,values) = (header,averageColumns values)

averageColumns :: [ ( T.Text , [ T.Text ] ) ] -> [( T.Text , Point )]
averageColumns = map averageColumn . filter (("SEQ" /=) . T.strip . fst)

averageColumn :: ( T.Text , [ T.Text ] ) -> ( T.Text , Point )
averageColumn = calcAverage . readColumn

readColumn :: ( T.Text , [ T.Text ] ) -> ( T.Text , [ Double ] )
readColumn (label,values) = (T.strip label, map readFloat values)
    where readFloat s = read (T.unpack s) :: Double

calcAverage :: ( T.Text , [ Double ] ) -> ( T.Text , Point )
calcAverage (label,values) = (label, getMeanSDandRSD $ tail values ) -- this is a crude solution to excluding the first value in a run.

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
