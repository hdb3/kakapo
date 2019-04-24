{-# LANGUAGE OverloadedStrings #-}
module Summarise where
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Sections hiding (main)

type Point = (Int,Double,Double,Double)
type DataPoint = ( [(T.Text , T.Text) ] , [( T.Text , Point) ])

main :: IO ()
main = processSection . fst . getSection . T.lines <$> T.getContents >>= print

processSection :: Section -> DataPoint
processSection (header,values,_) = (header,averageColumns values)

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
