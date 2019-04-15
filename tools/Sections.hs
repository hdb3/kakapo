module Sections where

import Data.Char (isSpace)
import Data.List (transpose)
import Control.Exception(assert)

main = do
   infile <- getContents
   print $ getSection infile

-- extended version of fields which allows for quoted text in a field, thereby allowing commas in quoted text
-- a subtle challenge is that we want spaces outside the quotes to be discarded....
-- for spaces at the beginning this requires to drop spaces when recognizing start quotes, which makes the quoted text guard more complex
-- an alternate option is to simply discard all leading and trailing spaces - this feels simpler.....

qFields :: String -> [String]
qFields s = let
    trim = dropWhile isSpace
    backTrim = takeWhile (not . isSpace) . trim
    isComma c = ',' == c
    in case (trim . dropWhile isComma . trim) s of
               "" -> []
               ('"' : s') -> w : qFields (tail s'') where (w, s'') = break ('"' ==) s'
               s' -> backTrim w : qFields s'' where (w, s'') = break isComma s'

getSection :: String -> ( [(String,String)] , [(String,[String])] , [(String,String)] )
getSection s =
    let getLine :: String -> String -> [String]
        getLine tag l = let fx = qFields l in assert ( tag == head fx ) $ tail fx 
        getLines :: String -> [String] -> ([[String]],[String])
        getLines t lx = let fxx = getLines' t lx 
                            getLines' tag ( l : lx ) =  let fx = qFields l in if tag == head fx then tail fx : getLines' tag lx else [ ( l : lx ) ]
                        in (init fxx , last fxx)


        getSingleRecord :: String -> [String] -> ([(String,String)], [String])
        getSingleRecord tag ( keyline : valline : rest ) = let keys = getLine "HDR" keyline 
                                                               values = getLine tag valline
                                                           in assert ( length keys == length values ) ( zip keys values, rest )

        getMultiRecord :: String -> [String] -> ([(String,[String])], [String])
        getMultiRecord tag ( keyline : vallines ) =
            let keys = getLine "HDR" keyline
                ( values, rest ) = getLines tag vallines
                tvalues = transpose values
            in assert ( length keys == length ( head tvalues ) ) ( zip (getLine "HDR" keyline ) tvalues , rest )

        getStart = getSingleRecord "START"
        getEnd   = getSingleRecord "END"
        getData   = getMultiRecord "DATA"

        (start,rest) = getStart ( lines s )
        (columns,rest') = getData rest
        (end,rest'') = getEnd rest'

    in (start,columns,end)
