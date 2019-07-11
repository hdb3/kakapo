{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(unless,when)
import Data.Either(partitionEithers)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Maybe(fromMaybe)
import System.Environment(getArgs)
import System.Exit(exitSuccess)
import GenParse(getData)
import Summary
import Constraints
import Graph
import AddMeta
--renderer = sdGraph -- allMeans / standardGraph / sdGraph / maxminGraph

main :: IO ()
main = do

   (optArgs , selectArgs) <- Data.List.partition (Data.List.isPrefixOf "--") . tail <$> getArgs

   let valueOpts s = map (drop (length s')) $ filter (Data.List.isPrefixOf s') optArgs where s' = "--" ++ s ++ "="
       valueOpt s | null opts = Nothing
                  | otherwise = Just $ head opts
                  where opts = valueOpts s
 
   if null selectArgs then

       putStrLn "please provide a selector to continue analysis"

   else if "--help" `elem` optArgs then do

         putStrLn constraintsHelp
         putStrLn "for example: 'z samples TOPIC=BASE1M MAXBURSTCOUNT=? PLATFORM=,'"
         putStrLn "options:"
         putStrLn "         --source : show source files"
         putStrLn "         --err : report files which failed to parse"
         putStrLn "         --erronly : only report files which failed to parse, then exit"
         putStrLn "         --min : plot minimum values from each sample rather than mean"

   else do

       (errors,samples') <- partitionEithers <$> getData
       let samples = updateDict addRibsize samples'

       when ("--err" `elem` optArgs || "--erronly" `elem` optArgs)
            (do mapM_ putStrLn errors
                putStrLn "\n++++++++++++++++++\n"
            )

       when ("--erronly" `elem` optArgs)
            exitSuccess

       fullReport "base" ["SOURCE","START","TIME","UUID","PID"] samples
       shortReport "base" samples

       putStrLn "\n++++++++++++++++++\n"

       let renderer = if "--min" `elem` optArgs then sdMinGraph else sdGraph

       let constraints = map getConstraint selectArgs
       selector <- buildSelector constraints
       putStrLn $ "Selector is " ++ showSelector selector

       let graph = preSelect selector samples
           subgraphs = Map.toAscList $ partition selector graph
           validSampleCount = length graph
           --validSampleCount = sum $ map ( length . snd ) graphs
           originalSampleCount = length samples
           graphSummary = map (\(l,sx) -> "(" ++ T.unpack l ++ " , " ++ show (length sx) ++ ")") subgraphs
           controlConstraints = querySelector isControl selector
           controlHeaders = filter (flip elem controlConstraints . fst) $ concatMap fst graph
           controlSummary = summariseMap controlHeaders

       putStrLn $ "selected samples / original samples : " ++ show validSampleCount ++ " / " ++ show originalSampleCount
       shortReport "selected" graph
       when ( 1 < length subgraphs )
            ( do putStrLn "sub-graphs generated"
                 putStrLn $ "graphSummary\n" ++ unlines graphSummary
                 mapM_ (\(t,sx) -> shortReport (T.unpack t) sx) subgraphs
            )

       unless (null controlConstraints)
              ( putStrLn "Controlled Parameters" >> putStrLn (displayMap controlSummary) )

       T.writeFile "plot.csv" $ renderer (Constraints.select selector graph)

       when ("--source" `elem` optArgs)
            ( do let getSource sample = maybe "source not found" T.unpack ( lookup (T.pack "SOURCE") ( fst sample ) )
                     selection = map2FlatList (Constraints.select selector graph) -- `asTypeOf` _
                 putStrLn $ "source report (" ++ show (length selection) ++ ")"
                 putStrLn $ unlines $ map (\(ta,tb,sample) -> (T.unpack ta ++ " , " ++ T.unpack tb ++ " , " ++ getSource sample)) selection )

       let topic = drop (length ("TOPIC="::String)) $ head $ filter (Data.List.isPrefixOf ("TOPIC="::String)) selectArgs
           gnuplotTitle = fromMaybe topic (valueOpt "title")
           gnuplotXLabel = T.unpack $ head controlConstraints
           gnuplotOutput s = "set output '" ++ gnuplotTitle ++ "." ++ s ++ "'"
           gnuplotPDF = "set terminal pdfcairo ; " ++ gnuplotOutput "pdf"
           gnuplotPNG = "set terminal pngcairo size 1500,1000 ; " ++ gnuplotOutput "png"
           gnuplotQT = [ "pause -1" ]
           errorBars = "--eb" `elem` optArgs || "--errorbars" `elem` optArgs
           gnuplotEB = ", '' using 1:2:3 with errorbars" 
           gnuplotCommon = [ "set yrange [0:]"
                           , "set rmargin at screen 0.95"
                           , "set ylabel 'RTT'"
                           , "set xlabel '" ++ gnuplotXLabel ++ "'"
                           , "set title '" ++ gnuplotTitle ++ "'"
                           , "plot for [i=0:*] 'plot.csv' index i using 1:2 with linespoint title columnheader(1)" ++ if errorBars then gnuplotEB else ""
                           ] 
           gnuplot commands = putStrLn $ "gnuplot -e \"" ++ Data.List.intercalate " ; " commands ++ "\""
        
       when ("--pdf" `elem` optArgs)
            ( gnuplot ( gnuplotPDF : gnuplotCommon ) )
        
       when ("--png" `elem` optArgs)
            --( putStrLn $ "\"" ++ Data.List.intercalate " ; " ( gnuplotPNG : gnuplotCommon ) ++ "\"" )
            ( gnuplot ( gnuplotPNG : gnuplotCommon ) )
        
       when ("--qt" `elem` optArgs)
            --( putStrLn $ "\"" ++ Data.List.intercalate " ; " ( gnuplotCommon ++ gnuplotQT ) ++ "\"" )
            ( gnuplot ( gnuplotCommon ++ gnuplotQT ) )
