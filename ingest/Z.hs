{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(unless,when)
import Data.Either(partitionEithers)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import System.Environment(getArgs)
import GenParse(getData)
import Summary
import Constraints
import Graph
import C2
renderer = sdGraph -- allMeans / standardGraph / sdGraph / maxminGraph
main = do
   (l,samples) <- partitionEithers <$> getData
   mapM_ putStrLn l
   putStrLn "\n++++++++++++++++++\n"

   fullReport "base" ["SOURCE","START","TIME"] samples
   shortReport "base" samples

   putStrLn "\n++++++++++++++++++\n"

   selectArgs <- tail <$> getArgs
   if null selectArgs
   then
       putStrLn "please provide a selector to continue analysis"
   else do
       let constraints = map getConstraint selectArgs
       selector <- buildSelector constraints
       putStrLn $ "Selector is " ++ showSelector selector

       let graph = C2.select selector samples
           subgraphs = Map.toAscList $ C2.partition selector graph
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
