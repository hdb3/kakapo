{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either(partitionEithers)
import Data.List (elem)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment(getArgs)
import Control.Arrow(first)
import GenParse(Dict,Sample,getData)
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
       fullReport "/" ["SOURCE","START","TIME","UUID"] r
       putStrLn ""
       shortReport "/" r
   else do
       let constraints = map getConstraint selectArgs
       selector <- buildSelector constraints
       let graphs = Constraints.select selector r
           graphSummary = map (\(l,sx) -> "(" ++ T.unpack l ++ " , " ++ show (length sx) ++ ")") $ map2list graphs
           variables = selectorVariables selector
           fixedPoints = selectorFixedPoints selector
           combinedHeaders = pruneHeaders fixedPoints $ map snd $ concatMap snd $ map2list graphs

       fullReport "selected" ["SOURCE","START","TIME","UUID"] combinedHeaders
       shortReport "selected" combinedHeaders
       putStrLn $ "graphSummary\n" ++ unlines graphSummary
       putStrLn $ "Selector is " ++ showSelector selector
       putStrLn $ T.unpack $ "Selector variables " +++ T.unwords variables
       putStrLn $ T.unpack $ "Selector fixed points " +++ T.unwords fixedPoints

       --T.writeFile "plot.csv" $ maxminGraph graphs
       T.writeFile "plot.csv" $ sdGraph graphs
       --T.writeFile "plot.csv" $ allMeans graphs
       --T.writeFile "plot.csv" $ standardGraph graphs

pruneHeaders :: [Text] -> [Sample] -> [Sample]
pruneHeaders labels = map (first (pruneHeader labels) )
    where
    pruneHeader :: [Text] -> Dict -> Dict
    pruneHeader labels = filter (not . flip elem labels . fst)
