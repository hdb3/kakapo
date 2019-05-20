{-# LANGUAGE OverloadedStrings #-}
module Graph where
import Data.List(intercalate,sortOn)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Arrow(second)

import GenParse(Sample,Dict,Metrics,rtt)
import qualified Mean

--type Dict = [(Text,Text)]
--type Sample = (Dict, Metrics)
type SelectResultList = [(Text, [(Text,Sample)])]

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

graph :: (Metrics -> [Double]) -> ([Double] -> Text) -> SelectResultList -> Text
graph metric reduction = l6 . l5 . l4 .l3 .l2 . l1
    where
        l1 = map snd
        l2 = mapmap (\(xPoint,(_,metrics)) -> (xPoint,metrics))
        l3 = mapmap (second metric)
        l4 = mapmap (second reduction)
        l5 = map ( sortOn (readInt . fst))
        l6 = T.intercalate "\n\n" . map T.unlines . mapmap (\(x,yx) -> x `T.append` " , " `T.append` yx)
        mapmap = map . map
        readInt :: Text -> Int
        readInt = read . T.unpack

standardGraph :: SelectResultList -> Text
standardGraph = graph rtt mean

mean :: [Double] -> Text
mean = T.pack . show . Mean.mean

gnuplot :: Text -> IO()
gnuplot _ = return ()

