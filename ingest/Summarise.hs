{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either(partitionEithers)
import Data.List (sortOn,elem)
import Data.Text(Text)
import qualified Data.Text as T
import System.Environment(getArgs)
import Control.Arrow(second)
import GenParse(getData)
import Summary
import Constraints

-- continue after failure?
barf = putStrLn
--barf = die

main = do
   (l,r) <- partitionEithers <$> getData
   mapM_ barf l
   selectArgs <- tail <$> getArgs
   let constraints = map getConstraint selectArgs
   let headers = concatMap fst r
       summary = Summary.summarise headers
   if
       null constraints
   then
       analyse summary
   else do
       putStrLn $ unlines $ map show constraints
       selector <- buildSelector constraints
       print (head r)
       putStrLn ""
       let t = Constraints.inner selector emptySelectResult (head r)
       print t

analyse :: [(Text, [(Text,Int)])] -> IO ()
analyse hdrs = do
   let shdrs = sortOn fst $ remove ["START","TIME","UUID","VERSION"] hdrs
       sshdrs = map (second (reverse . sortOn snd)) shdrs
   mapM_ (putStrLn . display) sshdrs

   where
       remove ks = filter ( not . contains ks . fst )
       contains set elt = elt `elem` set
       display (k,vs) = T.unpack k ++ " : " ++ show (length vs) ++ " { " ++ unwords ( map show' vs) ++ " }"
       show' (t,i) = T.unpack t ++ "[" ++ show i ++ "]"
