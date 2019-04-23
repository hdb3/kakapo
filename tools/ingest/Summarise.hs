{-# LANGUAGE OverloadedStrings #-}
module Summarise where
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List(intercalate)
import Data.Maybe(fromMaybe,fromJust)
import Sections hiding (main)

-- type Section = ( [(Text,Text)] , [(Text,[Text])] , [(Text,Text)] )
type DataPoint = ( [(T.Text , T.Text) ] , [( T.Text , (Int,Double,Double,Double) )])
main = do
   infile <- T.lines  <$> T.getContents

   --let ((header,values,_), _) = getSection infile
   let dataPoint = processSection $ fst $ getSection infile 
   print dataPoint
   --print $ processSection (header,values)

--processSection :: ( [(T.Text , T.Text) ] , [( T.Text , [ T.Text ] )]) -> ( [(T.Text , T.Text) ] , [( T.Text , (Int,Double,Double,Double) )])
--processSection :: Section -> ( [(T.Text , T.Text) ] , [( T.Text , (Int,Double,Double,Double) )])
processSection :: Section -> DataPoint
processSection (header,values,_) = (header,averageColumns values)

averageColumns :: [ ( T.Text , [ T.Text ] ) ] -> [( T.Text , (Int,Double,Double,Double) )]
averageColumns = map averageColumn . filter (("SEQ" /=) . T.strip . fst)

averageColumn :: ( T.Text , [ T.Text ] ) -> ( T.Text , (Int,Double,Double,Double) )
averageColumn = ( calcAverage . readColumn )

readColumn :: ( T.Text , [ T.Text ] ) -> ( T.Text , [ Double ] )
readColumn (label,values) = (T.strip label, map readFloat values)
    where readFloat s = read (T.unpack s) :: Double
          readInt s = read (T.unpack s) :: Int

calcAverage :: ( T.Text , [ Double ] ) -> ( T.Text , (Int,Double,Double,Double) )
calcAverage (label,values) = (label, getMeanSDandRSD $ tail values ) -- this is a crude solution to excluding the first value in a run.

getMeanSDandRSD :: [Double] -> (Int,Double,Double,Double)
getMeanSDandRSD sample = 
    let
       count = fromIntegral $ length sample
       ssum = sum sample
       mean = ssum / count
       sqSum = foldl (\x y -> x + y * y) 0 sample
       sd    = sqrt ( (count * sqSum) - (ssum * ssum) ) / count
       rsd   = sd / mean 
    in (length sample, mean, sd, rsd)
