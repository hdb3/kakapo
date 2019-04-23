{-# LANGUAGE OverloadedStrings #-}
module Sections where
--module Sections(emptySection,getSection,Section) where

import Prelude hiding (getLine,rem)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (transpose)
import Control.Exception(assert)
import qualified QFields(parse)

type Section = ( [(Text,Text)] , [(Text,[Text])] , [(Text,Text)] )

qFields :: Text -> [Text]
qFields = QFields.parse
emptySection :: Section
emptySection = ([],[],[])

getSection :: [ Text ] -> (Section, [ Text] )
getSection tx =
    let getLine :: Text -> Text -> [Text]
        getLine tag l = let fx = qFields l in assert ( tag == head fx ) $ tail fx 
        getLines :: Text -> [Text] -> ([[Text]],[Text])
        getLines t' lx' = let fxx = getLines' t' lx' 
                        in (init fxx , last fxx)
                        where
                            getLines'  _ [] = []
                            getLines' tag ( l : lx ) =  let fx = qFields l in if tag == head fx then tail fx : getLines' tag lx else [ l : lx ]


        getSingleRecord :: Text -> [Text] -> ([(Text,Text)], [Text])
        getSingleRecord tag ( keyline : valline : rem ) = let keys = getLine "HDR" keyline 
                                                              values = getLine tag valline
                                                           in assert ( length keys == length values ) ( zip keys values, rem )

        getMultiRecord :: Text -> [Text] -> ([(Text,[Text])], [Text])
        getMultiRecord tag ( keyline : vallines ) =
            let keys = getLine "HDR" keyline
                ( values, rem ) = getLines tag vallines
                tvalues = transpose values
            in assert ( length keys == length ( head values ) ) ( zip (getLine "HDR" keyline ) tvalues , rem )

        getStart = getSingleRecord "START"
        getEnd   = getSingleRecord "STOP"
        getData   = getMultiRecord "DATA"

        (start,rest) = getStart tx
        (columns,rest') = getData rest
        (end,rest'') = getEnd rest'
{- *** why does the below consistency check loop? -}
        --(end,rest'') = assert (1 < length rest'') ( getEnd rest' )
        --goodEnd = assert  ( null rest'' || ( (not . null . fst . getStart) rest'' ))

    --in assert ( null rest'' || ( (not . null . fst . getStart) rest'' )) ( (start,columns,end) , rest'')
    in ( (start,columns,end) , rest'')
    -- in assert goodEnd (start,columns,end)

getSections :: Text -> [ Section ]
getSections = getSections' . T.lines
    where
    getSections' :: [ Text ] -> [ Section ]
    getSections' tx =
        let (section,more) = getSection tx
        in if null more then
               [ section ]
           else
               section : getSections' more

main = do
    content <- T.getContents
    --print $ getSection $ T.lines content
    print $ getSections content
