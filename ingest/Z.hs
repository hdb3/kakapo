{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either(partitionEithers)
import Data.List (sortOn,elem)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import System.Environment(getArgs)
import Control.Arrow(first,second)
import GenParse(Dict,Samples,getData)
import Summary
import Constraints
import Graph
import C2

main = do
   (l,samples) <- partitionEithers <$> getData
   mapM_ putStrLn l
   selectArgs <- tail <$> getArgs
   if null selectArgs
   then
       putStrLn "please provide a selector"
   else do
       let constraints = map getConstraint selectArgs
       selector <- buildSelector constraints
       let graphs = Map.toAscList $ C2.select selector samples
           validSampleCount = sum $ map ( length . snd ) graphs 
           originalSampleCount = length samples
           graphSummary = map (\(l,sx) -> "(" ++ T.unpack l ++ " , " ++ show (length sx) ++ ")") graphs
 
       --analyse combinedHeaders
       --analyse2 combinedHeaders
       putStrLn $ "Selector is " ++ showSelector selector
       putStrLn $ "selected samples / original samples : " ++ show validSampleCount ++ " / " ++ show originalSampleCount
       putStrLn $ "graphSummary\n" ++ unlines graphSummary 

       putStrLn $ "\n++++++++++++++++++\n"

       fullReport ["SOURCE","START","TIME"] samples
       shortReport samples

       putStrLn $ "\n++++++++++++++++++\n"

       mapM_ (\(t,sx) -> putStrLn (T.unpack t) >> shortReport sx) graphs

       --T.writeFile "plot.csv" $ allMeans graphs
       --T.writeFile "plot.csv" $ standardGraph graphs
