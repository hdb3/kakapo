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
       analyse r
       putStrLn ""
       analyse2 r
   else do
       let constraints = map getConstraint selectArgs
       selector <- buildSelector constraints
       let graphs = Map.toList $ Constraints.select selector r
           graphSummary = map (\(l,sx) -> "(" ++ T.unpack l ++ " , " ++ show (length sx) ++ ")") graphs
           variables = selectorVariables selector
           fixedPoints = selectorFixedPoints selector
           combinedHeaders = pruneHeaders fixedPoints $ map snd $ concatMap snd graphs
 
       analyse combinedHeaders
       analyse2 combinedHeaders
       putStrLn $ "graphSummary\n" ++ unlines graphSummary 
       putStrLn $ "Selector is " ++ showSelector selector
       putStrLn $ T.unpack $ "Selector variables " +++ T.unwords variables
       putStrLn $ T.unpack $ "Selector fixed points " +++ T.unwords fixedPoints

       T.writeFile "plot.csv" $ allMeans graphs

pruneHeaders :: [Text] -> Samples -> Samples
pruneHeaders labels = map (first (pruneHeader labels) )
    where
    pruneHeader :: [Text] -> Dict -> Dict
    pruneHeader labels = filter (not . flip elem labels . fst)

analyse2 :: Samples -> IO ()
analyse2 samples = do
    let hdrs = Summary.summarise $ concatMap fst samples
        count = length samples
    putStrLn $ show count ++ " samples found"
    let nlabels = length hdrs
    putStrLn $ show nlabels ++ " metadata labels found"
    let pInvariant = (1 ==) . length . snd
        pUnassociated = ((count `div` 2) < ) . length . snd
        pVariable x = not ( pInvariant x) && not ( pUnassociated x)
        invariants = filter pInvariant hdrs
        unassociated = filter pUnassociated hdrs
        variable = filter pVariable hdrs
    putStrLn $ show (length invariants) ++ " invariants found: " ++ unwords ( map ( T.unpack . fst )  invariants)
    putStrLn $ show (length unassociated) ++ " unassociated found: " ++ unwords ( map ( T.unpack . fst) unassociated)
    putStrLn $ show (length variable) ++ " variable found: " ++ unwords ( map ( T.unpack . fst) variable)

analyse :: Samples -> IO () 
analyse samples = do
   let hdrs = Summary.summarise $ concatMap fst samples
       sortedHeaders = map (second (reverse . sortOn snd)) $ sortOn fst hdrs
   mapM_ (putStrLn . display) sortedHeaders

   where
       --remove ks = filter ( not . contains ks . fst )
       --contains set elt = elt `elem` set
       display (k,vs) = T.unpack k ++ " : " ++ show (length vs) ++ " { " ++ unwords ( map show' vs) ++ " }"
       show' (t,i) = T.unpack t ++ "[" ++ show i ++ "]"
