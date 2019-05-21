{-#LANGUAGE OverloadedStrings #-}
module Main where

import Data.Either(partitionEithers)
import GenParse(getData)
import Summary

-- continue after failure?
barf = putStrLn
--barf = die

main = do
   (l,r) <- partitionEithers <$> getData
   mapM_ barf l
   let headers = concatMap fst r
       summary = Summary.summarise headers
   mapM_ print ( filter ( not . flip elem ["SOURCE","START","TIME"] . fst ) summary )
