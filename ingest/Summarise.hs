{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either(partitionEithers)
import Data.List (sortOn,elem)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Environment(getArgs)
import Control.Arrow(second)
import GenParse(Samples,getData)
import Summary
import Constraints

-- continue after failure?
barf = putStrLn
--barf = die

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
       --putStrLn $ unlines $ map show constraints
       selector <- buildSelector constraints
       let graphs = Map.toList $ Constraints.select selector r -- `asTypeOf` _
           graphSummary = map (\(l,sx) -> "(" ++ T.unpack l ++ " , " ++ show (length sx) ++ ")") graphs
           combinedHeaders = map snd $ concatMap snd graphs
 
       analyse combinedHeaders
       analyse2 combinedHeaders
       putStrLn $ "graphSummary\n" ++ unlines graphSummary 
       putStrLn $ "Selector is " ++ showSelector selector
       putStrLn $ "Selector variables " ++ unwords ( selectorVariables selector)

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
       sshdrs = map (second (reverse . sortOn snd)) $ sortOn fst hdrs
       -- sshdrs = map (second (reverse . sortOn snd)) $ sortOn fst $ remove ["START","TIME","UUID","VERSION"] hdrs
   mapM_ (putStrLn . display) sshdrs

   where
       remove ks = filter ( not . contains ks . fst )
       contains set elt = elt `elem` set
       display (k,vs) = T.unpack k ++ " : " ++ show (length vs) ++ " { " ++ unwords ( map show' vs) ++ " }"
       show' (t,i) = T.unpack t ++ "[" ++ show i ++ "]"
