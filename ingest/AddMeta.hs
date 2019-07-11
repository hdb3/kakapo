{-# LANGUAGE OverloadedStrings #-}
module AddMeta(updateDict,addRibsize) where

import qualified Data.Text as T
import Data.Text(Text)
import qualified Data.List as L
import Control.Arrow(first)
import Data.Maybe(fromMaybe)
import GenParse( Dict,Sample )

{-
Kakapo meta-metric derivation

Metrics for graph plotting may not be explicit in source metadata,
  the most obvious example being RIB size,
  which in current versions is BLOCKSIZE * GROUPSIZE * MAXBURSTCOUNT.
   A derived metadata type RIBSIZE would be valuable.
   This is about how that can be achieved by post processing.
   GenParse.getData exports sample of this form:
       type Dict = [(Text,Text)]
       type Sample = (Dict, Metrics)
We seek to extend values of type [Dict] with additional Dict elements, derived from one or more preexisting Dict values.
This can be expressed as the application of a function which ‘extracts’ the required values and synthesises a new one, e.g.
       let blocksize = readInt . lookUp “BLOCKSIZE”
           groupsize = readInt . lookUp “GROUPSIZE”
           maxburstcount = readInt . lookUp “MAXBURSTCOUNT”
       append (“RIBSIZE” , blocksize * groupsize * maxburstcount)

Here is the complete version

-}


type MetaAdd =  (Text -> Int) -> (Text -> Text) -> ((Text, Int) -> (Text,Text)) -> (Text,Text)
--addRibsize :: (Text -> Int) -> (Text -> Text) -> ((Text, Int) -> (Text,Text)) -> (Text,Text)
addRibsize :: MetaAdd
addRibsize readInt lookUp append =
    let blocksize = readInt $ lookUp "BLOCKSIZE"
        groupsize = readInt $ lookUp "GROUPSIZE"
        maxburstcount = readInt $ lookUp "MAXBURSTCOUNT"
    in append ("RIBSIZE" , blocksize * groupsize * maxburstcount)

--And the applying framework is :
apply :: MetaAdd -> Dict -> Dict
apply f dict =
    let readInt = read . T.unpack
        --lookUp k = unJust $ L.lookup k dict
        --unJust (Just x) = x
        lookUp k = fromMaybe (error $ "lookup failed for " ++ T.unpack k ++ " in " ++ show dict) ( L.lookup k dict )
        append (k,v) = (k, T.pack $ show v)
    in f readInt lookUp append : dict

updateDict :: MetaAdd -> [Sample] -> [Sample]
updateDict f = map (first (apply f))
