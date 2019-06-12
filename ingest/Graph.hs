{-# LANGUAGE OverloadedStrings #-}
module Graph where
import Text.Read(readMaybe)
import Data.Maybe(fromMaybe)
import Data.List(sortOn)
import Data.Text(Text)
import qualified Data.Text as T
import Control.Arrow(second)
import qualified Data.Map.Strict as Map
import Constraints(SelectResult,unmapmap)

import GenParse(Sample,Metrics,rtt)
import qualified Mean



{-
  graph - produce a gnuplot format multifile dataset from the generic 'SelectResult' container
    top level - extract the multiple graphs from the map, discarding the 'title' which is the selector without the type
              - Map.Map Text [(Text,Sample)] -> Text [(Text,Sample)]
    2nd level - reduce [(Text,Sample)] == [(Text,(Dict, Metrics))] to [(Text, Metrics)], discarding the headers
              - we now have [[(Text, Metrics)]], which is a complete result set for any given 'plot'
    3rd level - extract the metric of interest - e.g. rtt, --> [[(Text, [Float])]]
              - this provides a list of floats for each data point, which can be reduced in various ways.
              - the specific metric is provided as a function to enable generic application.
    4th level - a stereortypical reduction is 'mean' which is a single value
              - other reductions lead to other formats, with varying numbers of fields, e.g. adding standard deviation, or max and min 
              - thus this reduction is genrically represented as [Float] -> Text
              - and the graph is specialised by defining a specific reduction function
    5th level - sort the individual graphs over the numeric value of the x field
    6th level - completion - the graph is completed by joining the 'x' Text value with the output of the reduction, and concatenating the multiple graphs with
              - the gnuplot appropriate seprator ("\n\n"). 

    extension - an alternate path is the generation of a multi-file result where the basis for the graphs is the selection of two or more metrics over a singleton input Resultset.
              - the logic is similar, the distinction being that a list of metric extractors should be provided rather than one, and the input should only be singular
-}

graph :: (Metrics -> [Double]) -> ([Double] -> Text) -> SelectResult -> Text
graph metric reduction = l6 . l5 . l4 .l3 .l2 . l1 . unmapmap
    where
        l1 = map snd
        l2 = mapmap (\(xPoint,(_,metrics)) -> (xPoint,metrics))
        l3 = mapmap (second metric)
        l4 = mapmap (second reduction)
        l5 = map ( sortOn (readInt . fst))
        l6 = T.intercalate "\n\n" . map T.unlines . mapmap (\(x,yx) -> x `T.append` " " `T.append` yx)
        mapmap = map . map
        readInt :: Text -> Int
        readInt t = fromMaybe (error $ "readInt failed reading " ++ T.unpack t ++ ", perhaps you specified a non-numeric type as a control variable?")
                              ( readMaybe $ T.unpack t)
        --readInt = read . T.unpack

standardGraph :: SelectResult -> Text
standardGraph = graph rtt mean
   where
   mean :: [Double] -> Text
   mean = T.pack . show . Mean.mean

sdGraph :: SelectResult -> Text
sdGraph = graph rtt sdMean
   where
   sdMean :: [Double] -> Text
   sdMean = T.pack . (\(a,b) -> show a ++ " " ++ show b ) . Mean.meanSD

maxminGraph :: SelectResult -> Text
maxminGraph = graph rtt sdMean
   where
   sdMean :: [Double] -> Text
   sdMean = T.pack . (\(a,b) -> show a ++ " " ++ show b ) . Mean.meanSD

allMeans  :: SelectResult -> Text
allMeans =  graph rtt  ( (\(a,b,c,d) -> T.pack $ unwords $ map show [a,b,c,d]) . Mean.maxSDMinMean)

gnuplot :: Text -> IO()
gnuplot _ = return ()

