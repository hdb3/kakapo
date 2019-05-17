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
import Control.Arrow(second)
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

analyse :: [(Text, [(Text,Int)])] -> IO ()
analyse hdrs = do
   --mapM_ print hdrs
   let shdrs = sortOn fst $ remove ["START","TIME"] $ hdrs
       sshdrs = map (second (sortOn snd)) shdrs
   mapM_ print shdrs
   mapM_ putStrLn ( map display sshdrs)

   where
       remove ks = filter ( not . contains ks . fst )
       contains set elt = elem elt set
       --display (k,vs) = T.unpack k ++ " : " ++ show (length vs) ++ show vs
       display (k,vs) = T.unpack k ++ " : " ++ show (length vs) ++ " { " ++ unwords ( map show' vs) ++ " }"
       show' (t,i) = T.unpack t ++ "[" ++ show i ++ "]"
