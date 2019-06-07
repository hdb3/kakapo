{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either(partitionEithers)
import Data.List (elem)
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

-- continue after failure?
barf = putStrLn
--barf = die
(+++) = T.append
main = do
   (l,r) <- partitionEithers <$> getData
   mapM_ barf l
   selectArgs <- tail <$> getArgs
   if null selectArgs
   then do
       fullReport "/" ["SOURCE","START","TIME"] r
       putStrLn ""
       shortReport "/" r
   else do
       let constraints = map getConstraint selectArgs
       selector <- buildSelector constraints
       let graphs = map (second Map.toAscList) $ Map.toAscList $ Constraints.select selector r
           graphSummary = map (\(l,sx) -> "(" ++ T.unpack l ++ " , " ++ show (length sx) ++ ")") graphs
           variables = selectorVariables selector
           fixedPoints = selectorFixedPoints selector
           combinedHeaders = pruneHeaders fixedPoints $ map snd $ concatMap snd graphs
 
       fullReport "selected" ["SOURCE","START","TIME"] combinedHeaders
       shortReport "selected" combinedHeaders
       putStrLn $ "graphSummary\n" ++ unlines graphSummary 
       putStrLn $ "Selector is " ++ showSelector selector
       putStrLn $ T.unpack $ "Selector variables " +++ T.unwords variables
       putStrLn $ T.unpack $ "Selector fixed points " +++ T.unwords fixedPoints

       T.writeFile "plot.csv" $ maxminGraph graphs
       --T.writeFile "plot.csv" $ sdGraph graphs
       --T.writeFile "plot.csv" $ allMeans graphs
       --T.writeFile "plot.csv" $ standardGraph graphs

pruneHeaders :: [Text] -> Samples -> Samples
pruneHeaders labels = map (first (pruneHeader labels) )
    where
    pruneHeader :: [Text] -> Dict -> Dict
    pruneHeader labels = filter (not . flip elem labels . fst)
