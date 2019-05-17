{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Either(partitionEithers)
import Data.Maybe(fromMaybe,isJust,fromJust)
import Data.List (sortOn,transpose,elem)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Control.Applicative((<|>))
import System.Exit(die)
import System.Environment(getArgs)
import GenParse(getData,Dict,Metrics)
import Summary

-- continue after failure?
barf = putStrLn
--barf = die

main = do
   (l,r) <- partitionEithers <$> getData
   mapM_ barf l
   let headers = concatMap fst r
       summary = Summary.summarise headers
   analyse summary

analyse hdrs = do
   --mapM_ print hdrs
   let shdrs = sortOn fst $ remove ["START","TIME"] $ hdrs
   mapM_ print shdrs

   where
       remove ks = filter ( contains ks . fst )
       contains set elt = elem elt set
