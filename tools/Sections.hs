module Sections(emptySection,getSection,Section) where

import Prelude hiding (getLine,rem)
import Data.Char (isSpace)
import Data.List (transpose)
import Control.Exception(assert)

-- extended version of fields which allows for quoted text in a field, thereby allowing commas in quoted text
-- a subtle challenge is that we want spaces outside the quotes to be discarded....
-- for spaces at the beginning this requires to drop spaces when recognizing start quotes, which makes the quoted text guard more complex
-- an alternate option is to simply discard all leading and trailing spaces - this feels simpler.....

type Section = ( [(String,String)] , [(String,[String])] , [(String,String)] )

emptySection :: Section
emptySection = ([],[],[])

qFields :: String -> [String]
qFields s = let
    trim = dropWhile isSpace
    backTrim = takeWhile (not . isSpace) . trim
    isComma c = ',' == c
    in case (trim . dropWhile isComma . trim) s of
               "" -> []
               ('"' : s') -> w : qFields (tail s'') where (w, s'') = break ('"' ==) s'
               s' -> backTrim w : qFields s'' where (w, s'') = break isComma s'

getSection :: String -> Section
getSection s =
    let getLine :: String -> String -> [String]
        getLine tag l = let fx = qFields l in assert ( tag == head fx ) $ tail fx 
        getLines :: String -> [String] -> ([[String]],[String])
        getLines t' lx' = let fxx = getLines' t' lx' 
                        in (init fxx , last fxx)
                        where
                            getLines'  _ [] = []
                            getLines' tag ( l : lx ) =  let fx = qFields l in if tag == head fx then tail fx : getLines' tag lx else [ ( l : lx ) ]


        getSingleRecord :: String -> [String] -> ([(String,String)], [String])
        getSingleRecord tag ( keyline : valline : rem ) = let keys = getLine "HDR" keyline 
                                                              values = getLine tag valline
                                                           in assert ( length keys == length values ) ( zip keys values, rem )

        getMultiRecord :: String -> [String] -> ([(String,[String])], [String])
        getMultiRecord tag ( keyline : vallines ) =
            let keys = getLine "HDR" keyline
                ( values, rem ) = getLines tag vallines
                tvalues = transpose values
            in assert ( length keys == length ( head values ) ) ( zip (getLine "HDR" keyline ) tvalues , rem )

        getStart = getSingleRecord "START"
        getEnd   = getSingleRecord "STOP"
        getData   = getMultiRecord "DATA"

        (start,rest) = getStart ( lines s )
        (columns,rest') = getData rest
        (end,rest'') = getEnd rest'
{- *** why does the below consistency check loop? -}
        --(end,rest'') = assert (1 < length rest'') ( getEnd rest' )
        --goodEnd = assert  ( null rest'' || ( (not . null . fst . getStart) rest'' ))

    in assert ( null rest'' || ( (not . null . fst . getStart) rest'' )) (start,columns,end)
    -- in assert goodEnd (start,columns,end)
